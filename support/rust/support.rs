#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(unused)]

// extern crate num_bigint;

use std::sync::Arc;
// use num_bigint::BigInt;

use IdrisValue::*;

#[derive(Debug)]
enum IdrisValue {
    Int(i64),
    // Integer(BigInt),
    Char(char),
    Double(f64),
    Str(String),
    Lambda(fn(Arc<IdrisValue>) -> Arc<IdrisValue>),
    Erased,
    DataCon { tag: u32, args: Vec<Arc<IdrisValue>> },
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

    fn unwrap_lambda(&self) -> &fn(Arc<IdrisValue>) -> Arc<IdrisValue> {
        if let Lambda(x) = self { x } else { panic!("Expected IdrisValue::Lambda") }
    }

    fn unwrap_data_con(&self) -> (&u32, &Vec<Arc<IdrisValue>>) {
        if let DataCon { tag, args } = self { (tag, args) } else { panic!("Expected IdrisValue::DataCon") }
    }
}
