'use strict';

// HELPERS

function idrisRTS_boolToInt(x) {
  return (x) ? 1 : 0;
}


// INT

function idrisRTS_intAdd(x, y, bits) {
  return (x + y) % Math.pow(2, bits);
}

function idrisRTS_intSub(x, y, bits) {
  return (x - y) % Math.pow(2, bits);
}

function idrisRTS_intMul(x, y, bits) {
  return (x * y) % Math.pow(2, bits);
}

function idrisRTS_intDiv(x, y, bits) {
  return (x / y) % Math.pow(2, bits);
}

function idrisRTS_intMod(x, y) {
  return x % y;
}

function idrisRTS_intNeg(x) {
  return -x;
}

function idrisRTS_intShl(x, y) {
  return x << y;
}

function idrisRTS_intShr(x, y) {
  return x >> y;
}


// STRING

function idrisRTS_stringLength(str) {
  return str.length;
}

function idrisRTS_stringHead(str) {
  return str[0];
}

function idrisRTS_stringTail(str) {
  return str.slice(1);
}

function idrisRTS_stringIndex(str, index) {
  return str[index];
}

function idrisRTS_stringCons(chr, str) {
  return chr + str;
}

function idrisRTS_stringAppend(str1, str2) {
  return str1 + str2;
}

function idrisRTS_stringReverse(str) {
  return str.split('').reverse().join('');
}

function idrisRTS_stringSubstr(str, offset, length) {
  return str.substr(offset, length);
}


// CASTS

function idrisRTS_integerToInt(x) {
  return x; // TODO: Implement for Integer
}

function idrisRTS_integerToDouble(x) {
  return x; // TODO: Implement for Integer
}

function idrisRTS_integerToString(x) {
  return x.toString();// TODO: Implement for Integer
}


function idrisRTS_intToInteger(x) {
  return x; // TODO: Implement for Integer
}

function idrisRTS_intToDouble(x) {
  return x;
}

function idrisRTS_intToChar(x) {
  return String.fromCharCode(x);
}

function idrisRTS_intToString(x) {
  return x.toString();
}


function idrisRTS_doubleToInteger(x) {
  return Math.trunc(x); // TODO: Implement for Integer
}

function idrisRTS_doubleToInt(x) {
  return Math.trunc(x);
}

function idrisRTS_doubleToString(x) {
  return x.toString();
}


function idrisRTS_charToInteger(x) {
  return x.charCodeAt(0); // TODO: Implement for Integer
}

function idrisRTS_charToInt(x) {
  return x.charCodeAt(0);
}

function idrisRTS_charToString(x) {
  return x.toString();
}


function idrisRTS_stringToInteger(x) {
  return parseInt(x); // TODO: Implement for Integer
}

function idrisRTS_stringToInt(x) {
  return parseInt(x);
}

function idrisRTS_stringToDouble(x) {
  return parseFloat(x);
}


// IO

function idrisRTS_putStr(str) {
  process.stdout.write(str);
  return { tag: 0, args: [] }; // MkUnit
}

function idrisRTS_getStr(str) {
  // TODO: Implement
  return '';
}
