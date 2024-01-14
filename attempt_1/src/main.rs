use std::{fmt::{Debug, Display}, ops::{Add, Index, Sub}};

use nannou::prelude::*;

type List<T> = Vec<T>;
#[allow(non_camel_case_types)]
type number = u8;

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Clone)]
struct Halves(u8);

impl Halves {
    fn from_whole(whole: u8) -> Self {
        Halves(whole * 2)
    }

    fn one_half() -> Self {
        Halves(1)
    }

    fn between(a: Self, b: Self) -> Self {
        assert!(i16::abs(a.0 as i16 - b.0 as i16) == 1, "can only move between adjacent tiles: {a} and {b} are not 1 apart");
        let Halves(a_halves) = a;
        let Halves(b_halves) = b;
        Halves((a_halves + b_halves) / 2)
    }

    fn is_round(self) -> bool {
        self.0 % 2 == 0
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
    moves: List<List<T>>,
    min_size: number,
    max_size: number,
    first_size: number,
}



#[derive(Copy,Clone,PartialEq,Eq,Hash,PartialOrd,Ord, Debug)]
enum Move {
    RR, 
    RL,
    LR,
    LL
}

struct PatternIter<'a> {
    pattern: &'a Pattern,
    coord: PatternCoord,
}

impl<'a> Iterator for PatternIter<'a> {
    type Item = (PatternCoord, Move);

    fn next(&mut self) -> Option<Self::Item> {
        let PatternIter { pattern, coord } = self;
        pattern.validate(*coord);

        let coord_clone = coord.clone();

        let PatternCoord { row, col } = coord;
        if *row >= pattern.moves.len() as number {
            return None;
        }

        let row_index = *row as usize;
        let col_index = (col.0 / 2) as usize;

        let move_ = pattern.moves[row_index][col_index];

        let result = Some((coord_clone, move_));

        if col_index == pattern.moves[row_index].len() - 1 {
            *row += 1;
            *col = if pattern.first_big() && *row % 2 == 1 {
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
        match self {
            RR => f.write_str("RR"),
            RL => f.write_str("RL"),
            LR => f.write_str("LR"),
            LL => f.write_str("LL"),
        }
    }
}


impl Pattern {
    fn new(moves: List<List<Move>>) -> Self {
        assert!(moves.len() > 2);

        let min_size = moves.iter().map(|row| row.len()).min().unwrap();
        let max_size = moves.iter().map(|row| row.len()).max().unwrap();
        let first_size = moves[0].len();

        assert!(moves.iter().all(|row| row.len() == min_size || row.len() == max_size), "all rows must be either {min_size} or {max_size} long");

        for i in 0..moves.len() - 1 {
            let row_len = moves[i].len() as i32;
            let next_row_len = moves[i+1].len() as i32;

            assert_eq!(
                i32::abs(row_len - next_row_len), 1,
                "row {} and row {} must be 1 apart in length", i, i+1
            );
        }

        let result = Pattern {
            moves: moves,
            min_size: min_size as number,
            max_size: max_size as number,
            first_size: first_size as number
        };
        result
    }

    fn first_big(&self) -> bool {
        self.first_size == self.max_size
    }

    fn validate(&self, coord: PatternCoord) {
        if self.first_big() {
            // e: . . .
            // o:  . .
            // e: . . .
            // o:  . .
            if coord.row % 2 == 0 {
                assert!(coord.col.is_round(), "even rows must be on round columns");
            } else {
                assert!(!coord.col.is_round(), "odd rows must be on half columns");
            }
        } else {
            // e:  . .
            // o: . . .
            // e:  . .
            // o: . . .
            if coord.row % 2 == 0 {
                assert!(!coord.col.is_round(), "even rows must be on round columns");
            } else {
                assert!(coord.col.is_round(), "odd rows must be on half columns");
            }
        }

    }

    fn iter(&self) -> PatternIter {
        PatternIter {
            pattern: self,
            coord: PatternCoord {
                row: 0,
                col: if self.first_big() {
                    Halves::from_whole(0)
                } else {
                    Halves::one_half()
                }
            }
        }

    }
}

impl Index<PatternCoord> for Pattern {
    type Output = Move;

    fn index(&self, coord: PatternCoord) -> &Self::Output {
        self.validate(coord);
        &self.moves[coord.row as usize][(coord.col.0 / 2) as usize] // always positive, so no worries about rounding
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct PatternCoord {
    row: number,
    col: Halves
}

impl Debug for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut prev = None;
        for (coord, move_) in self.iter() {
            if coord.row == 0 && coord.col.0 <= 1 || Some(coord.row) != prev.map(|prev: PatternCoord| prev.row) {
                write!(f, "\n")?;
                if coord.col.is_round() {
                    write!(f, "  ")?;
                } else {
                    write!(f, "      ")?;
                }
            }
            // for debugging indices
            //write!(f, " ({}, {}) ", coord.row, coord.col)?;
            write!(f, " {move_} ")?;

            prev = Some(coord);
        }
        write!(f, "\n")?;
        Ok(())
    }
}

use Move::*;



fn main() {
    let test_pattern = Pattern::new( vec![
        vec![LL, LL, RR, LL, LL],
          vec![LL, LL, LL, LL],
        vec![LL, LL, LL, LL, LL],
          vec![LL, LL, LL, RR],
        vec![LL, LL, LL, LL, LL],
    ]);
    println!("valid test pattern: {test_pattern:#?}");
    //nannou::sketch(view).run()
}

fn apply_pattern(pattern: &Pattern, initial_colors: Vec<Color>) {
    assert_eq!(initial_colors.len(), pattern.max_size as usize * 2);



}


fn view(app: &App, frame: Frame) {
    // Begin drawing
    let win = app.window_rect();
    let t = app.time;
    let draw = app.draw();

    // Clear the background to black.
    draw.background().color(BLACK);

    // Create an `ngon` of points.
    let n_points = 5;
    let radius = win.w().min(win.h()) * 0.25;
    let points = (0..n_points).map(|i| {
        let fract = i as f32 / n_points as f32;
        let phase = fract;
        let x = radius * (TAU * phase).cos();
        let y = radius * (TAU * phase).sin();
        pt2(x, y)
    });
    draw.polygon()
        .x(-win.w() * 0.25)
        .color(WHITE)
        .rotate(-t * 0.1)
        .stroke(PINK)
        .stroke_weight(20.0)
        .join_round()
        .points(points);

    // Do the same, but give each point a unique colour.
    let n_points = 7;
    let points_colored = (0..n_points).map(|i| {
        let fract = i as f32 / n_points as f32;
        let phase = fract;
        let x = radius * (TAU * phase).cos();
        let y = radius * (TAU * phase).sin();
        let r = fract;
        let g = 1.0 - fract;
        let b = (0.5 + fract) % 1.0;
        (pt2(x, y), rgb(r, g, b))
    });
    draw.polygon()
        .x(win.w() * 0.25)
        .rotate(t * 0.2)
        .points_colored(points_colored);

    // Write the result of our drawing to the window's frame.
    draw.to_frame(app, &frame).unwrap();
}
