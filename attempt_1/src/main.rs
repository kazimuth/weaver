use std::{fmt::{Debug, Display}, ops::{Add, Index, Sub}, num::NonZeroU8};
use ahash::AHashSet;

use nannou::prelude::*;

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
        assert!(number::abs(a.0 - b.0) == 1, "can only move between adjacent tiles: {a} and {b} are not 1 apart");
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
struct ColorVar {
    id: number,
}
impl Debug for ColorVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("c{}", self.id))
    }
}

#[derive(Clone)]
struct Pattern<T: Copy> {
    tiles: List<List<T>>,
    min_size: number,
    max_size: number,
    first_size: number,
}

type ThreadID = NonZeroU8;

struct ThreadGrid {
    width: number,
    height: number,
    positions: List<List<Option<ThreadID>>>
}

impl ThreadGrid {
    fn new<T: Copy>(pattern: Pattern<T>) -> Self {
        let width = pattern.max_size * 2;
        let height = pattern.tiles.len() as number + 1;
        let positions = vec![vec![None; width as usize]; height as usize];

        ThreadGrid {
            width,
            height,
            positions
        }
    }

    fn get(&self, coord: ThreadCoord) -> Option<ThreadID> {
        assert!(coord.row.0 % 2 == 1, "coord.row must be aligned to a quarter, not a half or whole: {:?}", coord);
        assert!(coord.col.0 % 2 == 1, "coord.row must be aligned to a quarter, not a half or whole: {:?}", coord);
        // the coordinate system is: index [0,0] corresponds to [-0.25, -0.25]; index [1, 1] corrseponds to 
        let row = (coord.row + Quarters::one_quarter()).0 / 2;
        let col = (coord.col + Quarters::one_quarter()).0 / 2;

        self.positions[row as usize][col as usize]
    }

    fn set(&mut self, coord: ThreadCoord, id: Option<ThreadID>) {
        assert!(coord.row.0 % 2 == 1, "coord.row must be aligned to a quarter, not a half or whole: {:?}", coord);
        assert!(coord.col.0 % 2 == 1, "coord.row must be aligned to a quarter, not a half or whole: {:?}", coord);
        // the coordinate system is: index [0,0] corresponds to [-0.25, -0.25]; index [1, 1] corrseponds to 
        let row = (coord.row + Quarters::one_quarter()).0 / 2;
        let col = (coord.col + Quarters::one_quarter()).0 / 2;

        self.positions[row as usize][col as usize] = id
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


}

#[derive(Debug)]
struct ThreadCoord {
    row: Quarters,
    col: Quarters
}


impl<T: Copy> Pattern<T> {
    fn new(tiles: List<List<T>>) -> Self {
        assert!(tiles.len() > 2);

        let min_size = tiles.iter().map(|row| row.len()).min().unwrap();
        let max_size = tiles.iter().map(|row| row.len()).max().unwrap();
        let first_size = tiles[0].len();

        assert!(tiles.iter().all(|row| row.len() == min_size || row.len() == max_size), "all rows must be either {min_size} or {max_size} long");

        for i in 0..tiles.len() - 1 {
            let row_len = tiles[i].len() as i32;
            let next_row_len = tiles[i+1].len() as i32;

            assert_eq!(
                i32::abs(row_len - next_row_len), 1,
                "row {} and row {} must be 1 apart in length", i, i+1
            );
        }

        let result = Pattern {
            tiles: tiles,
            min_size: min_size as number,
            max_size: max_size as number,
            first_size: first_size as number
        };
        result
    }

    fn first_big(&self) -> bool {
        self.first_size == self.max_size
    }

    fn validate(&self, coord: TileCoord) {
        if self.first_big() {
            // e: . . .
            // o:  . .
            // e: . . .
            // o:  . .
            assert_eq!(coord.row.is_round(), coord.col.is_round(), "first big: even and odd rows must both be whole or both fractions, {coord:?}");
        } else {
            // e:  . .
            // o: . . .
            // e:  . .
            // o: . . .
            assert_eq!(coord.row.is_round(), !coord.col.is_round(), "first small: one of even and odd must be whole, the other fractional, {coord:?}");
        }

    }
}

impl<'a, T: Copy> IntoIterator for &'a Pattern<T> {
    type Item = (TileCoord, T);
    type IntoIter = PatternIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        PatternIter {
            pattern: self,
            coord: TileCoord {
                row: Halves::from_whole(0),
                col: if self.first_big() {
                    Halves::from_whole(0)
                } else {
                    Halves::one_half()
                }
            }
        }
    }
}

impl<T: Copy> Index<TileCoord> for Pattern<T> {
    type Output = T;

    fn index(&self, coord: TileCoord) -> &Self::Output {
        self.validate(coord);
        &self.tiles[coord.row.round_down() as usize][coord.col.round_down() as usize]
    }
}

impl<T: Display + Copy> Debug for Pattern<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut prev = None;
        for (coord, tile_) in self {
            if (coord.row == Halves::from_whole(0) && coord.col.times_two() <= 1) || Some(coord.row) != prev.map(|prev: TileCoord| prev.row) {
                write!(f, "\n")?;
                if coord.col.is_round() {
                    write!(f, "  ")?;
                } else {
                    write!(f, "   ")?;
                }
            }
            // for debugging indices
            //write!(f, " ({}, {}) ", coord.row, coord.col)?;
            //write!(f, " ({}, {}) ", coord.row.times_two(), coord.col.round_down())?;
            write!(f, " {tile_}")?;

            prev = Some(coord);
        }
        write!(f, "\n")?;
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct TileCoord {
    row: Halves,
    col: Halves
}

struct PatternIter<'a, T: Copy> {
    pattern: &'a Pattern<T>,
    coord: TileCoord,
}

impl<'a, T: Copy> Iterator for PatternIter<'a, T> {
    type Item = (TileCoord, T);

    fn next(&mut self) -> Option<Self::Item> {
        let PatternIter { pattern, coord } = self;
        pattern.validate(*coord);

        let coord_clone = coord.clone();

        let TileCoord { row, col } = coord;
        if row.times_two() >= pattern.tiles.len() as number {
            return None;
        }

        let row_index = row.times_two() as usize;
        let col_index = col.round_down() as usize;

        let tile_ = pattern.tiles[row_index][col_index];

        let result = Some((coord_clone, tile_));

        if col_index == pattern.tiles[row_index].len() - 1 {
            // end of line.
            *row = *row + Halves::one_half();

            // :-)
            *col = if pattern.first_big() ^ row.is_round() { 
                Halves::one_half()
            } else {
                Halves::from_whole(0)
            };
        } else {
            *col = *col + Halves::from_whole(1);
        }

        result
    }
}

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

#[derive(Copy,Clone,PartialEq,Eq,Hash,PartialOrd,Ord, Debug)]
enum Move {
    RR, 
    RL,
    LR,
    LL
}

use Move::*;


fn main() {
    let test_pattern = Pattern::new( vec![
        vec![LL, LL, RR, LL, LL],
          vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LR, LL],
          vec![LL, LL, LL, RR],
        vec![LL, LL, RL, LL, LL],
          vec![LL, LL, LL, RR],
        vec![LL, LL, LL, LL, LL],
    ]);
    println!("valid test pattern: {test_pattern:#?}");
    let test_pattern = Pattern::new( vec![
          vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
          vec![LL, LL, LL, RR],
        vec![LL, LL, LL, LL, LL],
          vec![LL, LL, LL, RR],
        vec![LL, LL, LL, LL, LL],
    ]);
    println!("valid test pattern: {test_pattern:#?}");

    nannou::app(model).update(update).run();
}


struct Model {
    _window: window::Id,
    font: text::Font,
    pattern: Pattern<Move>
}

fn model(app: &App) -> Model {
    let _window = app.new_window().view(view).build().unwrap();
    Model {
        _window,
        pattern: Pattern::new( vec![
            vec![LL, LL, RR, LL, LL],
              vec![LL, LL, LL, LL],
            vec![LL, LL, LL, LR, LL],
              vec![LL, LL, LL, RR],
            vec![LL, LL, RL, LL, LL],
              vec![LL, LL, LL, RR],
            vec![LL, LL, LL, LL, LL],
        ]),
        font: text::Font::from_bytes(include_bytes!("../FiraCode-Retina.ttf")).unwrap(),
    }
}

fn update(_app: &App, _model: &mut Model, _update: Update) {}

fn view(app: &App, model: &Model, frame: Frame) {
   // Begin drawing
    let win = app.window_rect();
    let t = app.time;
    // drawing in window space
    let draw = app.draw();

        // Clear the background to black.
    draw.background().color(BLACK);

    draw.text("x=0,y=100")
        .color(WHITE)
        .x_y(0.0, 100.0);

    let translation = Vec2::new(-0.0, -0.0);
    let scaling = 70.0;

    // pattern 2 window
    // note: pattern's y is flipped and it goes down
    let p2w = |pattern_coord: Point2| -> Point2 {
        let mut coord = pattern_coord * scaling;
        coord.y = -coord.y;
        coord + translation
    };
    let w2p = |window_coord: Point2| -> Point2 {
        let mut coord = window_coord - translation;
        coord.y = -coord.y;
        coord / scaling
    };

    let tilefrac = 0.7;
    assert!(0.5 <= tilefrac && tilefrac <= 1.0);

    // Draw tiles
    let to_top = vec2(0.0, 0.5);
    let to_right = vec2(0.5, 0.0);
    let to_bottom = vec2(0.0, -0.5);
    let to_left = vec2(-0.5, 0.0);
    let to_top_corner = to_top * tilefrac;
    let to_right_corner = to_right * tilefrac;
    let to_bottom_corner = to_bottom * tilefrac;
    let to_left_corner = to_left * tilefrac;

    for (coord, move_) in &model.pattern {
        let center = pt2(coord.col.into(), coord.row.into());
        let points = [p2w(center + to_top_corner), p2w(center + to_right_corner), p2w(center + to_bottom_corner), p2w(center + to_left_corner)];

        let color = if coord.col.is_round() {
            rgb(100u8,100,100)
        } else {
            rgb(50, 50, 50)
        };

        draw.polygon()
            .color(color)
            .stroke_weight(0.01)
            .stroke(WHITE)
            .points(points);

        let window_center = p2w(center);
        draw.text(move_.into())
            .color(WHITE)
            .font_size(15)
            .font(model.font.clone())
            .x_y(window_center.x, window_center.y);

    }

    draw.to_frame(app, &frame).unwrap();
}
