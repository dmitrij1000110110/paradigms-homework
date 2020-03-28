"use strict";

const cnst = value => () => value;
const one = cnst(1);
const two = cnst(2);
const VARS = {
    "x": 0,
    "y": 1,
    "z": 2
};
const variable = (variableName) => {
    const index = VARS[variableName];
    return (...args) => args[index];
};

const operation = operation => (...funcs) => (...params) => operation(...funcs.map(f => f(...params)));
const negate = operation(x => -x);
const abs = operation(Math.abs);
const add = operation((x, y) => x + y);
const subtract = operation((x, y) => x - y);
const multiply = operation((x, y) => x * y);
const divide = operation((x, y) => x / y);
const iff = operation((x, y, z) => x >= 0 ? y : z);
const OPERATIONS = {
    "+": [add, 2],
    "-": [subtract, 2],
    "*": [multiply, 2],
    "/": [divide, 2],
    "negate": [negate, 1],
    "abs": [abs, 1],
    "iff": [iff, 3]
};
const CONSTS = {
    "one": one,
    "two": two
};

function parse(expression) {
    let stack = [];
    expression.split(" ").filter(x => x.length > 0).forEach(token => {
        if (token in OPERATIONS) {
            const curOperation = OPERATIONS[token];
            stack.push(curOperation[0](...stack.splice(-curOperation[1])));
        } else if (token in CONSTS) {
            stack.push(CONSTS[token]);
        } else if (token in VARS) {
            stack.push(variable(token));
        } else {
            stack.push(cnst(+token));
        }
    });
    return stack.pop()
}


// test programm
const testExpr = parse("x x 2 - * x * 1 +");
for (let i = 0; i < 11; i++) {
    console.log(testExpr(i))
}
