"use strict";

function createTerm(obj, toString, evaluate, diff) {
    obj.prototype.toString = toString;
    obj.prototype.evaluate = evaluate;
    obj.prototype.diff = diff;
}


function Const(value) {
    this.value = value;
}

const CONSTS = {
    0: new Const(0),
    1: new Const(1)
};

createTerm(Const,
    function () {
        return "" + this.value;
    },
    function () {
        return +this.value;
    },
    () => CONSTS[0]
);


const VARS = {
    "x": 0,
    "y": 1,
    "z": 2
};

function Variable(variableName) {
    this.variableName = variableName;
    this.argIndex = VARS[variableName];
}

createTerm(Variable,
    function () {
        return this.variableName;
    },
    function (...args) {
        return args[this.argIndex];
    },
    function (diffVariable) {
        return this.variableName === diffVariable ? CONSTS[1] : CONSTS[0];
    }
);

function Operation(...args) {
    this.args = args;
}

createTerm(Operation,
    function () {
        return this.args.map(f => f.toString()).join(" ") + " " + this.operand;
    },
    function (...params) {
        return this.operation(...this.args.map(f => f.evaluate(...params)));
    },
    function (diffVariable) {
        return this.diffFunction(diffVariable, ...this.args)
    }
)

function createOperation(operand, operation, diffFunction) {
    const obj = function (...args) {
        Operation.call(this, ...args);
    };
    obj.prototype = Object.create(Operation.prototype);
    obj.prototype.operand = operand;
    obj.prototype.operation = operation;
    obj.prototype.diffFunction = diffFunction;
    obj.nargs = operation.length;
    return obj;
}

const Negate = createOperation("negate", a => -a,
    function (diffVariable, a) {
        return new Negate(a.diff(diffVariable));
    });

const Add = createOperation("+", (a, b) => a + b,
    (diffVariable, a, b) => new Add(a.diff(diffVariable), b.diff(diffVariable)), 2);


const Subtract = createOperation("-", (a, b) => a - b,
    (diffVariable, a, b) => new Subtract(a.diff(diffVariable), b.diff(diffVariable)), 2);

const Multiply = createOperation("*", (a, b) => a * b,
    (diffVariable, a, b) => new Add(
        new Multiply(a.diff(diffVariable), b),
        new Multiply(a, b.diff(diffVariable))
    ));


const Divide = createOperation("/", (a, b) => a / b,
    (diffVariable, a, b) => new Divide(
        new Subtract(
            new Multiply(a.diff(diffVariable), b),
            new Multiply(a, b.diff(diffVariable))
        ),
        new Multiply(b, b)));

const Gauss = createOperation("gauss", (a, b, c, x) => a * Math.exp(-(x - b) * (x - b) / c / c / 2),
    (diffVariable, a, b, c, x) => {
        const substr = new Subtract(x, b);
        return new Add(
            new Gauss(a.diff(diffVariable), b, c, x),
            new Multiply(new Gauss(a, b, c, x),
                new Negate(new Divide(
                    new Multiply(
                        substr,
                        substr
                    ),
                    new Multiply(
                        new Const(2),
                        new Multiply(
                            c,
                            c
                        )
                    )
                )).diff(diffVariable)
            )
        )
    });

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
            stack.push(new curOperation(...stack.splice(-curOperation.nargs)));
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
