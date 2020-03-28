"use strict";

function Const(value) {
    this.value = value;
}
Const.prototype.toString = function () {
    return "" + this.value;
};
Const.prototype.evaluate = function () {
    return +this.value;
};
Const.prototype.diff = function () {
    return new Const(0);
};
const CONSTS = {
    0: new Const(0),
    1: new Const(1)
};

const VARS = {
    "x": 0,
    "y": 1,
    "z": 2
};

function Variable(variableName) {
    this.variableName = variableName;
    this.argIndex = VARS[variableName];
}

Variable.prototype.toString = function () {
    return this.variableName;
};
Variable.prototype.evaluate = function (...args) {
    return args[this.argIndex];
};
Variable.prototype.diff = function (diffVariable) {
    return this.variableName === diffVariable ? CONSTS[1] : CONSTS[0];
};

function Operation(operand, operation, ...args) {
    this.operand = operand;
    this.operation = operation;
    this.args = args;
}

Operation.prototype.evaluate = function (...params) {
    return this.operation(...this.args.map(f => f.evaluate(...params)));
};
Operation.prototype.toString = function () {
    return this.args.map(f => f.toString()).join(" ") + " " + this.operand;
};

function Negate(x) {
    Operation.call(this, "negate", (a) => -a, x);
}

Negate.prototype = Object.create(Operation.prototype);
Negate.prototype.diff = function (diffVariable) {
    return new Negate(this.args[0].diff(diffVariable));
};

function Add(x, y) {
    Operation.call(this, "+", (a, b) => a + b, x, y);
}

Add.prototype = Object.create(Operation.prototype);
Add.prototype.diff = function (diffVariable) {
    return new Add(...this.args.map(arg => arg.diff(diffVariable)));
};

function Subtract(x, y) {
    Operation.call(this, "-", (a, b) => a - b, x, y);
}

Subtract.prototype = Object.create(Operation.prototype);
Subtract.prototype.diff = function (diffVariable) {
    return new Subtract(...this.args.map(arg => arg.diff(diffVariable)));
};

function Multiply(x, y) {
    Operation.call(this, "*", (a, b) => a * b, x, y);
}

Multiply.prototype = Object.create(Operation.prototype);
Multiply.prototype.diff = function (diffVariable) {
    return new Add(
        new Multiply(this.args[0].diff(diffVariable), this.args[1]),
        new Multiply(this.args[0], this.args[1].diff(diffVariable))
    );
};

function Divide(x, y) {
    Operation.call(this, "/", (a, b) => a / b, x, y);
}

Divide.prototype = Object.create(Operation.prototype);
Divide.prototype.diff = function (diffVariable) {
    return new Divide(
        new Subtract(
            new Multiply(this.args[0].diff(diffVariable), this.args[1]),
            new Multiply(this.args[0], this.args[1].diff(diffVariable))
        ),
        new Multiply(this.args[1], this.args[1]));
};

function Gauss(a, b, c, x) {
    Operation.call(this, "gauss", (a, b, c, x) => a * Math.exp(-(x - b) * (x - b) / c / c / 2),
        a, b, c, x);
}

Gauss.prototype = Object.create(Operation.prototype);
Gauss.prototype.diff = function (diffVariable) {
    return new Add(
        new Gauss(this.args[0].diff(diffVariable), this.args[1], this.args[2], this.args[3]),
        new Multiply(new Gauss(this.args[0], this.args[1], this.args[2], this.args[3]),
            new Negate(new Divide(
                new Multiply(
                    new Subtract(this.args[3], this.args[1]),
                    new Subtract(this.args[3], this.args[1])
                ),
                new Multiply(
                    new Const(2),
                    new Multiply(
                        this.args[2],
                        this.args[2]
                    )
                )
            )).diff(diffVariable)
        )
    )
};

const OPERATIONS = {
    "+": Add,
    "-": Subtract,
    "*": Multiply,
    "/": Divide,
    "negate": Negate,
    "gauss": Gauss
};

function parse(expression) {
    let stack = [];
    expression.split(" ").filter(x => x.length > 0).forEach(token => {
        if (token in OPERATIONS) {
            const curOperation = OPERATIONS[token];
            stack.push(new curOperation(...stack.splice(-curOperation.length)));
        } else if (token in VARS) {
            stack.push(new Variable(token));
        } else {
            stack.push(new Const(+token));
        }
    });
    return stack.pop()
}

// test programm
// const t = new Add(new Variable('x'), new Const(2)).diff('x');
// console.log(t);
// const testExpr = parse('x y z 0 gauss x y z 1 gauss 1 2 3 x gauss x y + y z - z x * x 3 / gauss gauss');
// console.log(testExpr);
// for (let i = 0; i < 11; i++) {
//     console.log(testExpr.evaluate(1, 0, 0));
// }
// const p = new Add(new Const(1), new Const(1));
// console.log(p.evaluate);
