// TODO:
// this would be much less painful on a diagonal coordinate system...

use ahash::{AHashMap, AHashSet};
use std::{
    fmt::{Debug, Display},
    ops::{Add, Index, IndexMut, Sub},
    sync::Arc,
};

use nannou::{color, event::WindowEvent, prelude::*};

type List<T> = Vec<T>;
#[allow(non_camel_case_types)]
type number = i16;

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Clone)]
struct Halves(number);

impl Halves {
    fn from_whole(whole: number) -> Self {
        Halves(whole * 2)
    }

    fn one_half() -> Self {
        Halves(1)
    }

    fn between(a: Self, b: Self) -> Self {
        assert!(
            number::abs(a.0 - b.0) == 1,
            "can only move between adjacent tiles: {a} and {b} are not 1 apart"
        );
        let Halves(a_halves) = a;
        let Halves(b_halves) = b;
        Halves((a_halves + b_halves) / 2)
    }

    fn is_round(self) -> bool {
        self.0 % 2 == 0
    }

    fn round_down(self) -> i16 {
        // https://stackoverflow.com/questions/62144550/how-to-round-a-number-up-or-down-in-rust
        self.0 / 2
    }

    fn times_two(self) -> i16 {
        self.0
    }
}

impl Into<f32> for Halves {
    fn into(self) -> f32 {
        let Halves(halves) = self;
        halves as f32 / 2.0
    }
}

impl Debug for Halves {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Halves(halves) = self;
        f.write_fmt(format_args!("{}", *halves as f32 / 2.0))
    }
}

impl Display for Halves {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Halves(halves) = self;
        f.write_fmt(format_args!("{}", *halves as f32 / 2.0))
    }
}

impl Add for Halves {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let Halves(lhs_halves) = self;
        let Halves(rhs_halves) = rhs;
        Halves(lhs_halves + rhs_halves)
    }
}

impl Sub for Halves {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let Halves(lhs_halves) = self;
        let Halves(rhs_halves) = rhs;
        Halves(lhs_halves - rhs_halves)
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Clone)]
struct Quarters(number);
impl Quarters {
    fn from_halves(halves: Halves) -> Self {
        let Halves(halves) = halves;
        Quarters(halves * 2)
    }

    fn from_whole(whole: number) -> Self {
        Quarters(whole * 4)
    }

    fn one_quarter() -> Self {
        Quarters(1)
    }
}

impl Into<f32> for Quarters {
    fn into(self) -> f32 {
        let Quarters(quarters) = self;
        quarters as f32 / 4.0
    }
}

impl Add<Quarters> for Quarters {
    type Output = Self;

    fn add(self, rhs: Quarters) -> Self::Output {
        let Quarters(lhs_quarters) = self;
        let Quarters(rhs_quarters) = rhs;
        Quarters(lhs_quarters + rhs_quarters)
    }
}
impl Sub<Quarters> for Quarters {
    type Output = Self;

    fn sub(self, rhs: Quarters) -> Self::Output {
        let Quarters(lhs_quarters) = self;
        let Quarters(rhs_quarters) = rhs;
        Quarters(lhs_quarters - rhs_quarters)
    }
}

impl Add<Halves> for Quarters {
    type Output = Self;

    fn add(self, rhs: Halves) -> Self::Output {
        let Quarters(lhs_quarters) = self;
        let Quarters(rhs_quarters) = Quarters::from_halves(rhs);
        Quarters(lhs_quarters + rhs_quarters)
    }
}

impl Debug for Quarters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Quarters(quarters) = self;
        f.write_fmt(format_args!("{}", *quarters as f32 / 4.0))
    }
}
impl Display for Quarters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Quarters(quarters) = self;
        f.write_fmt(format_args!("{}", *quarters as f32 / 4.0))
    }
}

#[derive(Clone)]
struct Pattern<T: Copy> {
    tiles: List<List<T>>,
    min_size: number,
    max_size: number,
    first_size: number,
}

impl<T: Copy> Pattern<T> {
    fn new(tiles: List<List<T>>) -> Self {
        assert!(tiles.len() > 2);

        let min_size = tiles.iter().map(|row| row.len()).min().unwrap();
        let max_size = tiles.iter().map(|row| row.len()).max().unwrap();
        let first_size = tiles[0].len();

        assert!(
            tiles
                .iter()
                .all(|row| row.len() == min_size || row.len() == max_size),
            "all rows must be either {min_size} or {max_size} long"
        );

        for i in 0..tiles.len() - 1 {
            let row_len = tiles[i].len() as i32;
            let next_row_len = tiles[i + 1].len() as i32;

            assert_eq!(
                i32::abs(row_len - next_row_len),
                1,
                "row {} and row {} must be 1 apart in length",
                i,
                i + 1
            );
        }

        let result = Pattern {
            tiles: tiles,
            min_size: min_size as number,
            max_size: max_size as number,
            first_size: first_size as number,
        };
        result
    }

    fn first_big(&self) -> bool {
        self.first_size == self.max_size
    }

    fn valid(&self, coord: TileCoord) -> bool {
        let correct_parity = if self.first_big() {
            coord.row.is_round() == coord.col.is_round()
        } else {
            coord.row.is_round() != coord.col.is_round()
        };

        correct_parity
            && 0 <= coord.row.times_two()
            && coord.row.times_two() < self.tiles.len() as number
            && 0 <= coord.col.round_down()
            && coord.col.round_down() < self.tiles[coord.row.times_two() as usize].len() as number
    }

    fn validate(&self, coord: TileCoord) {
        if self.first_big() {
            // e: . . .
            // o:  . .
            // e: . . .
            // o:  . .
            assert_eq!(
                coord.row.is_round(),
                coord.col.is_round(),
                "first big: even and odd rows must both be whole or both fractions, {coord:?}"
            );
        } else {
            // e:  . .
            // o: . . .
            // e:  . .
            // o: . . .
            assert_eq!(
                coord.row.is_round(),
                !coord.col.is_round(),
                "first small: one of even and odd must be whole, the other fractional, {coord:?}"
            );
        }
    }
    fn coords(&self) -> PatternIter {
        PatternIter {
            rows: self.tiles.len() as _,
            max_width: self.max_size,
            first_big: self.first_big(),
            coord: TileCoord {
                row: Halves::from_whole(0),
                col: if self.first_big() {
                    Halves::from_whole(0)
                } else {
                    Halves::one_half()
                },
            },
        }
    }

    fn coords_starting_at(&self, coord: TileCoord) -> PatternIter {
        assert!(self.valid(coord));
        PatternIter {
            rows: self.tiles.len() as _,
            max_width: self.max_size,
            first_big: self.first_big(),
            coord
        }
    }
}

impl<T: Copy> Index<TileCoord> for Pattern<T> {
    type Output = T;

    fn index(&self, coord: TileCoord) -> &Self::Output {
        self.validate(coord);

        &self.tiles[coord.row.times_two() as usize][coord.col.round_down() as usize]
    }
}
impl<T: Copy> IndexMut<TileCoord> for Pattern<T> {
    fn index_mut(&mut self, coord: TileCoord) -> &mut Self::Output {
        self.validate(coord);

        &mut self.tiles[coord.row.times_two() as usize][coord.col.round_down() as usize]
    }
}

impl<T: Display + Copy> Debug for Pattern<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut prev = None;
        for coord in self.coords() {
            let tile_ = self[coord];

            if (coord.row == Halves::from_whole(0) && coord.col.times_two() <= 1)
                || Some(coord.row) != prev.map(|prev: TileCoord| prev.row)
            {
                write!(f, "\n")?;
                if coord.col.is_round() {
                    write!(f, "  ")?;
                } else {
                    write!(f, "   ")?;
                }
            }
            // for debugging indices
            write!(f, " ({}, {}) ", coord.row, coord.col)?;
            //write!(f, " ({}, {}) ", coord.row.times_two(), coord.col.round_down())?;
            //write!(f, " {tile_}")?;

            prev = Some(coord);
        }
        write!(f, "\n")?;
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct TileCoord {
    row: Halves,
    col: Halves,
}

struct PatternIter {
    rows: number,
    max_width: number,
    first_big: bool,
    coord: TileCoord,
}

impl Iterator for PatternIter {
    type Item = TileCoord;

    fn next(&mut self) -> Option<Self::Item> {
        let PatternIter {
            coord,
            rows,
            first_big,
            max_width,
        } = self;

        let coord_clone = coord.clone();

        let TileCoord { row, col } = coord;
        if row.times_two() >= *rows {
            return None;
        }

        let col_index = col.round_down() as usize;

        let result = Some(coord_clone);
        // :^)
        let offset_row = *first_big ^ row.is_round();

        //println!("");
        //println!("coord: {:?}", coord_clone);
        //println!("offset_row: {:?}", offset_row);

        if (offset_row && col_index as number == *max_width - 2)
            || (!offset_row && col_index as number == *max_width - 1)
        {
            // end of line.
            *row = *row + Halves::one_half();

            *col = if offset_row {
                // this row was offset, so the next shouldn't be
                Halves::from_whole(0)
            } else {
                // vice versa
                Halves::one_half()
            };
        } else {
            *col = *col + Halves::from_whole(1);
        }

        result
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
enum Move {
    RR,
    RL,
    LR,
    LL,
}
use Move::*;

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str((*self).into())
    }
}
impl Into<&'static str> for Move {
    fn into(self) -> &'static str {
        match self {
            RR => "↘",
            LL => "↙",
            RL => "↩",
            LR => "↪",
            //RL => "⮂",
            //LR => "⮀",
        }
    }
}

type ThreadID = u8;

struct ThreadGrid {
    width: number,
    height: number,
    positions: List<List<Option<ThreadID>>>,
}

impl ThreadGrid {
    fn new<T: Copy>(pattern: &Pattern<T>) -> Self {
        let width = pattern.max_size * 2;
        let height = pattern.tiles.len() as number + 1;
        let positions = vec![vec![None; width as usize]; height as usize];

        ThreadGrid {
            width,
            height,
            positions,
        }
    }

    fn get(&self, coord: ThreadCoord) -> Option<ThreadID> {
        assert!(
            coord.row.0.rem_euclid(2) == 1,
            "coord.row must be aligned to a quarter, not a half or whole: {:?}",
            coord
        );
        assert!(
            coord.col.0.rem_euclid(2) == 1,
            "coord.col must be aligned to a quarter, not a half or whole: {:?}",
            coord
        );
        // the coordinate system is: index [0,0] corresponds to [-0.25, -0.25]; index [1, 1] corrseponds to
        let row = (coord.row + Quarters::one_quarter()).0 / 2;
        let col = (coord.col + Quarters::one_quarter()).0 / 2;

        self.positions[row as usize][col as usize]
    }

    fn set(&mut self, coord: ThreadCoord, id: Option<ThreadID>) {
        assert!(
            coord.row.0.rem_euclid(2) == 1,
            "coord.row must be aligned to a quarter, not a half or whole: {:?}",
            coord
        );
        assert!(
            coord.col.0.rem_euclid(2) == 1,
            "coord.col must be aligned to a quarter, not a half or whole: {:?}",
            coord
        );
        // the coordinate system is: index [0,0] corresponds to [-0.25, -0.25]; index [1, 1] corrseponds to
        let row = (coord.row + Quarters::one_quarter()).0 / 2;
        let col = (coord.col + Quarters::one_quarter()).0 / 2;

        self.positions[row as usize][col as usize] = id
    }

    fn apply(
        &mut self,
        tile_coord: TileCoord,
        pattern: &Pattern<Move>,
        colors: &mut Pattern<Rgba<u8>>,
        color_mapping: &AHashMap<u8, Rgba<u8>>,
    ) {
        let center = ThreadCoord {
            row: Quarters::from_halves(tile_coord.row),
            col: Quarters::from_halves(tile_coord.col),
        };
        let top_left = ThreadCoord {
            row: center.row - Quarters::one_quarter(),
            col: center.col - Quarters::one_quarter(),
        };
        let top_right = ThreadCoord {
            row: center.row - Quarters::one_quarter(),
            col: center.col + Quarters::one_quarter(),
        };
        let bottom_left = ThreadCoord {
            row: center.row + Quarters::one_quarter(),
            col: center.col - Quarters::one_quarter(),
        };
        let bottom_right = ThreadCoord {
            row: center.row + Quarters::one_quarter(),
            col: center.col + Quarters::one_quarter(),
        };

        let in_left = self.get(top_left);
        let in_right = self.get(top_right);

        //println!("apply {tile_coord:?} {top_left:?}->{in_left:?} {top_right:?}->{in_right:?}");

        let move_ = pattern[tile_coord];

        let (out_left, out_right) = if move_ == Move::LL || move_ == Move::RR {
            (in_right, in_left)
        } else {
            (in_left, in_right)
        };

        self.set(bottom_left, out_left);
        self.set(bottom_right, out_right);

        let fill_color = if move_ == LL || move_ == LR {
            in_right.map(|id| color_mapping[&id]).unwrap_or_default()
        } else {
            in_left.map(|id| color_mapping[&id]).unwrap_or_default()
        };
        colors[tile_coord] = fill_color;

        let offset_row = pattern.first_big() ^ tile_coord.row.is_round();

        if offset_row {
            if tile_coord.col == Halves::one_half() {
                let copy_in = ThreadCoord {
                    row: top_left.row,
                    col: top_left.col - Quarters::from_halves(Halves::one_half()),
                };
                let copy_out = ThreadCoord {
                    row: bottom_left.row,
                    col: top_left.col - Quarters::from_halves(Halves::one_half()),
                };
                self.set(copy_out, self.get(copy_in));
            }
            //println!("edge: {}", (self.width / 2) - 2);
            if tile_coord.col.round_down() == (self.width / 2) - 2 {
                //println!("FIRING RIGHT {tile_coord:?}");
                let copy_in = ThreadCoord {
                    row: top_right.row,
                    col: top_right.col + Quarters::from_halves(Halves::one_half()),
                };
                let copy_out = ThreadCoord {
                    row: bottom_right.row,
                    col: top_right.col + Quarters::from_halves(Halves::one_half()),
                };
                self.set(copy_out, self.get(copy_in));
            }
        }

        if tile_coord.col == Halves::one_half() {
            // TODO: edges...
        }
    }

    fn no_repetitions(&self) -> bool {
        let mut set = AHashSet::new();
        for row in &self.positions {
            for id in row {
                if let Some(id) = id {
                    if set.contains(id) {
                        return false;
                    }
                    set.insert(id);
                }
            }
            set.clear();
        }
        true
    }

    fn coords(&self) -> ThreadCoords {
        ThreadCoords {
            threads: self,
            coord: ThreadCoord {
                row: Quarters::from_whole(0) - Quarters::one_quarter(),
                col: Quarters::from_whole(0) - Quarters::one_quarter(),
            },
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct ThreadCoord {
    row: Quarters,
    col: Quarters,
}

struct ThreadCoords<'a> {
    threads: &'a ThreadGrid,
    coord: ThreadCoord,
}

impl<'a> Iterator for ThreadCoords<'a> {
    type Item = ThreadCoord;

    fn next(&mut self) -> Option<Self::Item> {
        let ThreadCoords { threads, coord } = self;

        // yield this at the end
        let coord_cloned = coord.clone();

        let row_index = (coord.row + Quarters::one_quarter()).0 / 2;
        let col_index = (coord.col + Quarters::one_quarter()).0 / 2;

        //println!("{:?} {} {}", coord_cloned, row_index, col_index);

        if row_index >= threads.height as number {
            return None;
        }

        if col_index == threads.width as number - 1 {
            coord.row = coord.row + Halves::one_half();
            coord.col = Quarters::from_whole(0) - Quarters::one_quarter();
        } else {
            coord.col = coord.col + Halves::one_half();
        }

        Some(coord_cloned)
    }
}

fn main() {
    let test_pattern = Pattern::new(vec![
        vec![LL, LL, RR, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LR, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, RL, LL, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, RR, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LR, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, RL, LL, LL],
        vec![LL, LL, LL, RR],
    ]);
    println!("valid test pattern: {test_pattern:#?}");
    let test_pattern = Pattern::new(vec![
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, LL, LL, LL],
    ]);
    println!("valid test pattern: {test_pattern:#?}");

    nannou::app(model).update(update).event(event).run();
}

struct Model {
    _window: window::Id,
    font: text::Font,
    pattern: Pattern<Move>,
    tile_colors: Pattern<Rgba<u8>>,
    threads: ThreadGrid,
    steps: i32,
    steps_per_second: f32,
    iterator: Option<PatternIter>,
    color_mapping: AHashMap<u8, Rgba<u8>>,
    view_translation: Vec2,
    view_scaling: f32,
}

impl Model {
    fn pattern_to_window(&self, pattern_coord: Point2) -> Point2 {
        let mut coord = pattern_coord * self.view_scaling;
        coord.y = -coord.y;
        coord + self.view_translation
    }

    fn window_to_pattern(&self, window_coord: Point2) -> Point2 {
        let mut coord = window_coord - self.view_translation;
        coord.y = -coord.y;
        coord / self.view_scaling
    }

    /// There is almost certainly a better way to do this -- this fails to find a tile half the time.
    /// I think you need to use rotations.
    /// Oh well.
    fn find_adequate_tile(&self, window_coord: Point2) -> Option<TileCoord> {
        let mouse_pos = self.window_to_pattern(window_coord);
        let mouse_pos = TileCoord {
            row: Halves((mouse_pos.y * 2.0).round() as _),
            col: Halves((mouse_pos.x * 2.0).round() as _),
        };
        if self.pattern.valid(mouse_pos) {
            Some(mouse_pos)
        } else {
            None
        }
    }
}

fn model(app: &App) -> Model {
    let _window = app.new_window().view(view).build().unwrap();
    /*let pattern = Pattern::new(vec![
        vec![LL, LL, RR, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LR, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, RL, LL, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, RR, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LR, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, RL, LL, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, LL, LR, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, RL, LL, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, RR, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LR, LL],
        vec![LL, LL, LL, RR],
        vec![LL, LL, RL, LL, LL],
        vec![LL, LL, LL, RR],
    ]);*/
    let pattern = Pattern::new(vec![
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
        vec![LL, LL, LL, LL],
    ]);
    let tile_colors = Pattern::new(
        pattern
            .tiles
            .iter()
            .map(|row| row.iter().map(|_| rgba(0u8, 0, 0, 0)).collect())
            .collect(),
    );

    let mut threads = ThreadGrid::new(&pattern);
    let coords = threads.coords().collect::<Vec<_>>();

    for (i, coord) in coords.iter().take(10).enumerate() {
        threads.set(*coord, Some(i as _));
    }

    let iterator = pattern.coords();

    let mut color_mapping = AHashMap::default();
    /* bad palette 
    color_mapping.insert(0, rgba8(255, 0, 0, 255));
    color_mapping.insert(1, rgba8(0, 255, 0, 255));
    color_mapping.insert(2, rgba8(0, 0, 255, 255));
    color_mapping.insert(3, rgba8(255, 255, 0, 255));
    color_mapping.insert(4, rgba8(255, 0, 255, 255));
    color_mapping.insert(5, rgba8(0, 255, 255, 255));
    color_mapping.insert(6, rgba8(0, 255, 255, 255));
    color_mapping.insert(7, rgba8(255, 0, 255, 255));
    color_mapping.insert(8, rgba8(255, 255, 0, 255));
    color_mapping.insert(9, rgba8(0, 0, 255, 255));
    */
    //  #ff595e, #ff924c, #ffca3a, #c5ca30, #8ac926, #52a675, #1982c4, #4267ac, #6a4c93 and #b5a6c9.
    color_mapping.insert(0, rgba8(0xff, 0x59, 0x5e, 255));
    color_mapping.insert(1, rgba8(0xff, 0x92, 0x4c, 255));
    color_mapping.insert(2, rgba8(0xff, 0xca, 0x3a, 255));
    color_mapping.insert(3, rgba8(0xc5, 0xca, 0x30, 255));
    color_mapping.insert(4, rgba8(0x8a, 0xc9, 0x26, 255));
    color_mapping.insert(5, rgba8(0x52, 0xa6, 0x75, 255));
    color_mapping.insert(6, rgba8(0x19, 0x82, 0xc4, 255));
    color_mapping.insert(7, rgba8(0x42, 0x67, 0xac, 255));
    color_mapping.insert(8, rgba8(0x6a, 0x4c, 0x93, 255));
    color_mapping.insert(9, rgba8(0xb5, 0xa6, 0xc9, 255));

    Model {
        _window,
        pattern: pattern,
        tile_colors: tile_colors,
        threads: threads,
        font: text::Font::from_bytes(include_bytes!("../FiraCode-Retina.ttf")).unwrap(),
        steps: 0,
        steps_per_second: 60.0,
        iterator: Some(iterator),
        color_mapping: color_mapping,
        view_translation: Vec2::new(-0.0, 300.0),
        view_scaling: 70.0,
    }
}

fn update(_app: &App, _model: &mut Model, _update: Update) {
    if _app.time * _model.steps_per_second > _model.steps as f32 {
        _model.steps += 1;

        if let Some(it) = &mut _model.iterator {
            // safe to repeatedly attempt after reaching the end
            if let Some(next) = it.next() {
                _model.threads.apply(
                    next,
                    &_model.pattern,
                    &mut _model.tile_colors,
                    &_model.color_mapping,
                );
            }
        }

        //println!("step {}", _model.steps);
    }
}

fn event(app: &App, model: &mut Model, event: Event) {
    match event {
        Event::WindowEvent {
            id,
            simple: Some(WindowEvent::MousePressed(MouseButton::Left)),
        } => {
            if id == model._window {
                if let Some(tile_coord) = model.find_adequate_tile(app.mouse.position()) {
                    let Model {
                        _window,
                        pattern,
                        tile_colors,
                        threads,
                        iterator,
                        color_mapping,
                        ..
                    } = model;
                    if let Some(it) = iterator {
                        // finish existing weave
                        for next in it {
                            threads.apply(next, pattern, tile_colors, color_mapping);
                        }
                    }

                    let current_move = pattern[tile_coord];
                    let new_move = match current_move {
                        LL => LR,
                        LR => RL,
                        RL => RR,
                        RR => LL,
                    };
                    pattern[tile_coord] = new_move;
                    *iterator = Some(pattern.coords_starting_at(tile_coord));
                }
            }
        }
        _ => {}
    }
}

fn view(app: &App, model: &Model, frame: Frame) {
    // Begin drawing
    let win = app.window_rect();
    let t = app.time;
    // drawing in window space (for font reasons...)
    let draw = app.draw();

    // Clear the background to black.
    draw.background().color(BLACK);

    let tilefrac = 0.7;
    assert!(0.5 <= tilefrac && tilefrac <= 1.0);

    // Draw threads
    for thread_coord in model.threads.coords() {
        let center = pt2(thread_coord.col.into(), thread_coord.row.into());
        if let Some(color) = model.threads.get(thread_coord) {
            let color = model.color_mapping[&color];
            draw.line()
                .color(color)
                .weight(2.0)
                .start(model.pattern_to_window(center + vec2(0.0, 0.25)))
                .end(model.pattern_to_window(center + vec2(0.0, -0.25)));
        }
    }

    // Draw tiles
    let to_top = vec2(0.0, 0.5);
    let to_right = vec2(0.5, 0.0);
    let to_bottom = vec2(0.0, -0.5);
    let to_left = vec2(-0.5, 0.0);
    let to_top_corner = to_top * tilefrac;
    let to_right_corner = to_right * tilefrac;
    let to_bottom_corner = to_bottom * tilefrac;
    let to_left_corner = to_left * tilefrac;

    for coord in model.pattern.coords() {
        let move_ = model.pattern[coord];
        let center = pt2(coord.col.into(), coord.row.into());
        let points = [
            model.pattern_to_window(center + to_top_corner),
            model.pattern_to_window(center + to_right_corner),
            model.pattern_to_window(center + to_bottom_corner),
            model.pattern_to_window(center + to_left_corner),
        ];

        let color = model.tile_colors[coord];

        draw.polygon()
            .color(color)
            .points(points);

        
        let tile_greyness = color.red / 3 + color.blue / 3 + color.green / 3;
        let font_color = if color.alpha == 0 || tile_greyness < 128 {
            WHITE
        } else {
            BLACK
        };

        let window_center = model.pattern_to_window(center);
        draw.text(move_.into())
            .color(font_color)
            .font_size(15)
            .font(model.font.clone())
            .x_y(window_center.x, window_center.y);
    }

    if let Some(mouse_coord) = model.find_adequate_tile(app.mouse.position()) {
        let mouse_pos =
            model.pattern_to_window(pt2(mouse_coord.col.into(), mouse_coord.row.into()));
        draw.ellipse()
            .stroke(WHITE)
            .stroke_weight(1.0)
            .no_fill()
            .x_y(mouse_pos.x, mouse_pos.y)
            .radius(tilefrac / 2.0 * model.view_scaling);
    }

    draw.to_frame(app, &frame).unwrap();
}
