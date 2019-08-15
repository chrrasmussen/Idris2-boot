#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(unused)]

// extern crate num_bigint;

use std::str::FromStr;
use std::fmt;
use std::sync::Arc;
use std::io::{self, BufRead};
// use num_bigint::BigInt;

use IdrisValue::*;

enum IdrisValue {
    Int(i64),
    // Integer(BigInt),
    Char(char),
    Double(f64),
    Str(String),
    Lambda(Box<Fn(Arc<IdrisValue>) -> Arc<IdrisValue>>),
    DataCon { tag: u32, args: Vec<Arc<IdrisValue>> },
    Erased,
    World,
}

impl IdrisValue {
    fn unwrap_int(&self) -> &i64 {
        if let Int(x) = self { x } else { panic!("Expected IdrisValue::Int") }
    }

    // fn unwrap_integer(&self) -> &BigInt {
    //     if let Integer(x) = self { x } else { panic!("Expected IdrisValue::Integer") }
    // }

    fn unwrap_char(&self) -> &char {
        if let Char(x) = self { x } else { panic!("Expected IdrisValue::Char") }
    }

    fn unwrap_double(&self) -> &f64 {
        if let Double(x) = self { x } else { panic!("Expected IdrisValue::Double") }
    }

    fn unwrap_str(&self) -> &String {
        if let Str(x) = self { x } else { panic!("Expected IdrisValue::Str") }
    }

    fn unwrap_lambda(&self) -> &Box<Fn(Arc<IdrisValue>) -> Arc<IdrisValue>> {
        if let Lambda(x) = self { x } else { panic!("Expected IdrisValue::Lambda") }
    }

    fn unwrap_data_con(&self) -> (&u32, &Vec<Arc<IdrisValue>>) {
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
            DataCon { tag, args } => write!(f, "DataCon {{ tag: {}, args: {:?} }}", tag, args),
            Erased => write!(f, "Erased"),
            World => write!(f, "World"),
        }
    }
}

fn idris_rts_str_append(x: Arc<IdrisValue>, y: Arc<IdrisValue>) -> Arc<IdrisValue> {
    Arc::new(Str(format!("{}{}", x.unwrap_str(), y.unwrap_str())))
}

fn idris_rts_put_str(x: Arc<IdrisValue>) -> Arc<IdrisValue> {
    print!("{}", x.unwrap_str());
    Arc::new(Erased)
}

fn idris_rts_get_str() -> Arc<IdrisValue> {
    let stdin = io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();
    Arc::new(Str(line1))
}
