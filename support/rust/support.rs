#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(unused)]

// extern crate num_bigint;

use std::convert::TryFrom;
use std::char;
use std::str::FromStr;
use std::fmt;
use std::sync::Arc;
use std::io::{self, BufRead};
// use num_bigint::BigInt;
// use num_traits::Zero;
// use num_traits::cast::{FromPrimitive, ToPrimitive};

use IdrisValue::*;

pub enum IdrisValue {
    Int(i64),
    // Integer(BigInt),
    Char(char),
    Double(f64),
    Str(String),
    Lambda(Box<Fn(Arc<IdrisValue>) -> Arc<IdrisValue>>),
    Delay(Box<Fn() -> Arc<IdrisValue>>),
    DataCon { tag: u32, args: Vec<Arc<IdrisValue>> },
    Erased,
    World,
}

impl IdrisValue {
    pub fn unwrap_int(&self) -> &i64 {
        if let Int(x) = self { x } else { panic!("Expected IdrisValue::Int") }
    }

    // pub fn unwrap_integer(&self) -> &BigInt {
    //     if let Integer(x) = self { x } else { panic!("Expected IdrisValue::Integer") }
    // }

    pub fn unwrap_char(&self) -> &char {
        if let Char(x) = self { x } else { panic!("Expected IdrisValue::Char") }
    }

    pub fn unwrap_double(&self) -> &f64 {
        if let Double(x) = self { x } else { panic!("Expected IdrisValue::Double") }
    }

    pub fn unwrap_str(&self) -> &String {
        if let Str(x) = self { x } else { panic!("Expected IdrisValue::Str") }
    }

    pub fn unwrap_lambda(&self) -> &Box<Fn(Arc<IdrisValue>) -> Arc<IdrisValue>> {
        if let Lambda(x) = self { x } else { panic!("Expected IdrisValue::Lambda") }
    }

    pub fn unwrap_delay(&self) -> &Box<Fn() -> Arc<IdrisValue>> {
        if let Delay(x) = self { x } else { panic!("Expected IdrisValue::Delay") }
    }

    pub fn unwrap_data_con(&self) -> (&u32, &Vec<Arc<IdrisValue>>) {
        if let DataCon { tag, args } = self { (tag, args) } else { panic!("Expected IdrisValue::DataCon") }
    }
}

impl fmt::Debug for IdrisValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Int(x) => write!(f, "Int({})", x),
            // Integer(x) => write!(f, "Integer({})", x.to_string()),
            Char(x) => write!(f, "Char({})", x),
            Double(x) => write!(f, "Double({})", x),
            Str(x) => write!(f, "Str({})", x),
            Lambda(_) => write!(f, "Lambda(_)"),
            Delay(_) => write!(f, "Delay(_)"),
            DataCon { tag, args } => write!(f, "DataCon {{ tag: {}, args: {:?} }}", tag, args),
            Erased => write!(f, "Erased"),
            World => write!(f, "World"),
        }
    }
}


// STRING

// TODO: Do these String functions return a reference to the original String values? Should they make a clone instead?
// TODO: These functions operate on codepoints instead of graphemes.

pub fn idris_rts_str_length(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Int(x.unwrap_str().chars().count() as i64))
}

pub fn idris_rts_str_head(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Char(x.unwrap_str().chars().next().unwrap()))
}

pub fn idris_rts_str_tail(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Str(x.unwrap_str().chars().into_iter().skip(1).collect()))
}

pub fn idris_rts_str_index(x: Arc<IdrisValue>, index: Arc<IdrisValue>) -> Arc<IdrisValue> {
    let i = usize::try_from(*index.unwrap_int()).unwrap();
    Arc::new(Char(x.unwrap_str().chars().nth(i).unwrap()))
}

pub fn idris_rts_str_cons(x: Arc<IdrisValue>, y: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Str(format!("{}{}", x.unwrap_char(), y.unwrap_str())))
}

pub fn idris_rts_str_append(x: Arc<IdrisValue>, y: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Str(format!("{}{}", x.unwrap_str(), y.unwrap_str())))
}

pub fn idris_rts_str_reverse(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Str(x.unwrap_str().chars().rev().collect()))
}

pub fn idris_rts_str_substr(offset: Arc<IdrisValue>, length: Arc<IdrisValue>, x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    let o = usize::try_from(*offset.unwrap_int()).unwrap();
    let l = usize::try_from(*length.unwrap_int()).unwrap();
    Arc::new(Str(x.unwrap_str().chars().into_iter().skip(o).take(l).collect()))
}


// CASTS

// pub fn idris_rts_integer_to_int(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
//     Arc::new(Int(x.unwrap_integer().to_i64().unwrap_or(0)))
// }

// pub fn idris_rts_integer_to_double(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
//     Arc::new(Double(x.unwrap_integer().to_f64().unwrap_or(0.0)))
// }

// pub fn idris_rts_integer_to_string(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
//     Arc::new(Str(x.unwrap_integer().to_string()))
// }


// pub fn idris_rts_int_to_integer(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
//     Arc::new(Integer(FromPrimitive::from_i64(*x.unwrap_int()).unwrap_or(Zero::zero())))
// }

pub fn idris_rts_int_to_double(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Double(*x.unwrap_int() as f64))
}

pub fn idris_rts_int_to_char(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Char(char::from_u32(*x.unwrap_int() as u32).unwrap_or('�')))
}

pub fn idris_rts_int_to_str(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Str(x.unwrap_int().to_string()))
}


// pub fn idris_rts_double_to_integer(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
//     Arc::new(Integer(FromPrimitive::from_f64(*x.unwrap_double()).unwrap_or(Zero::zero())))
// }

pub fn idris_rts_double_to_int(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Int(*x.unwrap_double() as i64))
}

pub fn idris_rts_double_to_str(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Str(x.unwrap_double().to_string()))
}


// pub fn idris_rts_char_to_integer(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
//     Arc::new(Integer(FromPrimitive::from_u32(*x.unwrap_char() as u32).unwrap_or(Zero::zero())))
// }

pub fn idris_rts_char_to_int(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Int(*x.unwrap_char() as i64))
}

pub fn idris_rts_char_to_str(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Str(x.unwrap_char().to_string()))
}


// pub fn idris_rts_str_to_integer(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
//     Arc::new(Integer(BigInt::from_str(x.unwrap_str()).unwrap_or(Zero::zero())))
// }

pub fn idris_rts_str_to_int(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Int(i64::from_str(x.unwrap_str()).unwrap_or(0)))
}

pub fn idris_rts_str_to_double(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Double(f64::from_str(x.unwrap_str()).unwrap_or(0.0)))
}


// IO

pub fn idris_rts_put_str(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    print!("{}", x.unwrap_str());
    Arc::new(Erased)
}

pub fn idris_rts_get_str() -> Arc<IdrisValue> {
    let stdin = io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();
    Arc::new(Str(line1))
}
