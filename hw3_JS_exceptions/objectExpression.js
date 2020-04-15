"use strict";

function createTerm(obj, toString, prefix, postfix, evaluate, diff) {
    obj.prototype.toString = toString;
    obj.prototype.prefix = prefix;
    obj.prototype.postfix = postfix;
    obj.prototype.evaluate = evaluate;
    obj.prototype.diff = diff;
}

function Const(value) {
    this.value = value;
}

Const.ZERO = new Const(0);
Const.ONE = new Const(1);
Const.TWO = new Const(2);

createTerm(Const,
    function () {
        return "" + this.value;
    },
    function () {
        return "" + this.value;
    },
    function () {
        return "" + this.value;
    },
    function () {
        return +this.value;
    },
    () => Const.ZERO
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
    function () {
        return this.variableName;
    },
    function () {
        return this.variableName;
    },
    function (...args) {
        return args[this.argIndex];
    },
    function (v) {
        return this.variableName === v ? Const.ONE : Const.ZERO;
    }
);

function Operation(...args) {
    this.args = args;
}

createTerm(Operation,
    function () {
        return this.args.join(" ") + " " + this.operand;
    },
    function () {
        return "(" + this.operand + " " + this.args.map(f => f.prefix()).join(" ") + ")";
    },
    function () {
        return "(" + this.args.map(f => f.postfix()).join(" ") + " " + this.operand + ")";
    },
    function (...params) {
        return this.operation(...this.args.map(f => f.evaluate(...params)));
    },
    function (v) {
        return this.diffFunction(v, ...this.args)
    }
)

function createOperation(operand, operation, diffFunction) {
    function Obj(...args) {
        Operation.call(this, ...args);
    }

    Obj.prototype = Object.create(Operation.prototype);
    Obj.prototype.constructor = Obj;
    Obj.prototype.operand = operand;
    Obj.prototype.operation = operation;
    Obj.prototype.diffFunction = diffFunction;
    return Obj;
}

const Negate = createOperation("negate", a => -a,
    (v, a) => new Negate(a.diff(v))
);

const Add = createOperation("+", (a, b) => a + b,
    (v, a, b) => new Add(a.diff(v), b.diff(v))
);


const Subtract = createOperation("-", (a, b) => a - b,
    (v, a, b) => new Subtract(a.diff(v), b.diff(v))
);

const Multiply = createOperation("*", (a, b) => a * b,
    (v, a, b) => new Add(
        new Multiply(a.diff(v), b),
        new Multiply(a, b.diff(v))
    )
);


const Divide = createOperation("/", (a, b) => a / b,
    (v, a, b) => new Divide(
        new Subtract(
            new Multiply(a.diff(v), b),
            new Multiply(a, b.diff(v))
        ),
        new Multiply(b, b)
    )
);

const Gauss = createOperation("gauss", (a, b, c, x) => a * Math.exp(-(x - b) * (x - b) / c / c / 2),
    (v, a, b, c, x) => {
        const substr = new Subtract(x, b);
        return new Add(
            new Gauss(a.diff(v), b, c, x),
            new Multiply(new Gauss(a, b, c, x),
                new Negate(new Divide(
                    new Multiply(
                        substr,
                        substr
                    ),
                    new Multiply(
                        Const.TWO,
                        new Multiply(
                            c,
                            c
                        )
                    )
                )).diff(v)
            )
        )
    }
);

const Mean = createOperation("mean",
    (...args) => args.length === 0 ? 0 : args.reduce((a, b) => a + b, 0) / args.length,
    (v, ...args) => new Mean(...args.map(f => f.diff(v)))
);

const Var = createOperation("var", (...args) => {
        const mean = args.reduce((a, b) => a + b, 0) / args.length;
        return args.length === 0 ? 0 : args.reduce((a, b) => a + (b - mean) * (b - mean), 0) / args.length;
    },
    (v, ...args) => {
        const mean = new Mean(...args);
        const meanDiff = mean.diff(v);
        return new Multiply(Const.TWO,
            new Mean(...args.map(f =>
                    new Multiply(
                        new Subtract(f, mean),
                        new Subtract(f.diff(v), meanDiff)
                    )
                )
            )
        );
    }
);

const OPERATIONS = {
    "+": Add,
    "-": Subtract,
    "*": Multiply,
    "/": Divide,
    "negate": Negate,
    "gauss": Gauss,
    "mean": Mean,
    "var": Var
};

function parse(expression) {
    let stack = [];
    expression.split(" ").filter(x => x.length > 0).forEach(token => {
        if (token in OPERATIONS) {
            const curOperation = OPERATIONS[token];
            stack.push(new curOperation(...stack.splice(-curOperation.prototype.operation.length)));
        } else if (token in VARS) {
            stack.push(new Variable(token));
        } else {
            stack.push(new Const(+token));
        }
    });
    return stack.pop()
}

function ParsingError(message) {
    this.message = message;
}

ParsingError.prototype = Object.create(Error.prototype);
ParsingError.prototype.name = "ParsingError"
ParsingError.prototype.constructor = ParsingError;

function createParsingError(name, buildMessage) {
    function Error(...args) {
        ParsingError.call(this, buildMessage(...args));
    }

    Error.prototype = Object.create(ParsingError.prototype);
    Error.prototype.name = name;
    Error.prototype.constructor = ParsingError;
    return Error;
}

const MissingBracketError = createParsingError("MissingBracketError",
    (pos, foundToken) => "Expected ) at pos " + pos + " but found " + foundToken);
// try {
//     throw new MissingBracketError("0", "test");
// } catch (e) {
//     console.log(e instanceof MissingBracketError);
// }
const IncorrectOperationError = createParsingError("IncorrectOperationError",
    (pos, foundToken) => "Invalid operation token at pos " + pos + ", found '" + foundToken + "'");
const UnexpectedArgsCount = createParsingError("UnexpectedArgsCount",
    (pos, operand, foundCount, expectedCount) => "Expected " + expectedCount + " arguments for operation '"
        + operand + "' at pos " + pos + " but found " + foundCount);
const UnexpectedTokenError = createParsingError("UnexpectedTokenError",
    (pos, foundToken) => "Unexpected token '" + foundToken + "' at pos " + pos);


function BaseParser(source, separators = [], mode = s => s) {
    let _pos = 0;
    source = mode(source);
    this.getPos = () => _pos;
    this.incPos = () => _pos++;
    this.getMode = () => mode;
    this.getNext = (n = 1) => {
        return mode(source.slice(_pos, _pos += n));
    };
    this.getSource = () => source;
    this.isSeparator = (c) => separators.includes(c);
}

BaseParser.prototype.hasNext = function (n = 1) {
    return this.getPos() + n <= this.getSource().length;
}
BaseParser.prototype.skipWhitespaces = function () {
    const source = this.getSource();
    while (this.getPos() < source.length && source[this.getPos()].trim() === "") {
        this.incPos();
    }
}
BaseParser.prototype.test = function (expectedToken) {
    expectedToken = this.getMode()(expectedToken);
    this.skipWhitespaces();
    const source = this.getSource();
    const pos = this.getPos();
    if (expectedToken.length > source.length - pos) {
        return false;
    }
    let ans = true;
    for (let i = 0; i < expectedToken.length; i++) {
        ans = ans && (expectedToken[i] === source[pos + i]);
    }
    return ans;
}
BaseParser.prototype.parseToken = function () {
    this.skipWhitespaces();
    let token = "";
    const source = this.getSource();
    if (this.hasNext() && this.isSeparator(source[this.getPos()])) {
        token = this.getNext();
    } else {
        while (this.hasNext() && !(this.isSeparator(source[this.getPos()]))) {
            token += this.getNext();
        }
    }
    return this.getMode()(token);
}

function parseExpression(expression, mode) {
    const parser = new BaseParser(expression.trim(), [" ", "(", ")"],
        mode === "prefix" ? (s => s) : (s => s.split("").reverse().join("")));
    const openBracket = mode === "prefix" ? "(" : ")";
    const closeBracket = mode === "prefix" ? ")" : "(";

    function parseArgument(token) {
        let result;
        if (token === openBracket) {
            result = parseExpression();
            token = parser.parseToken();
            if (token !== closeBracket) {
                throw new MissingBracketError(parser.getPos(), token);
            }
        } else if (token in VARS) {
            result = new Variable(token);
        } else if (!isNaN(+token)) {
            result = new Const(+token);
        } else {
            throw new UnexpectedTokenError(parser.getPos(), token);
        }
        return result;
    }

    function parseExpression() {
        let token = parser.parseToken();
        if (!(token in OPERATIONS)) {
            throw new IncorrectOperationError(parser.getPos(), token);
        }
        const curOperation = OPERATIONS[token];
        let operationArgs = [];
        while (parser.hasNext() && !parser.test(closeBracket)) {
            token = parser.parseToken();
            operationArgs.push(parseArgument(token))
        }
        if (curOperation.prototype.operation.length !== 0 && operationArgs.length !== curOperation.prototype.operation.length) {
            throw new UnexpectedArgsCount(parser.getPos(), curOperation.prototype.operand,
                operationArgs.length, curOperation.prototype.operation.length);
        }
        return new curOperation(...(mode === "prefix" ? operationArgs : operationArgs.reverse()));
    }

    function parse() {
        if (expression.trim().length === 0) {
            throw new IncorrectOperationError(0, "");
        }
        let result = parseArgument(parser.parseToken());
        if (parser.hasNext()) {
            throw new ParsingError("Unexpected symbols at the end of expression");
        } else {
            return result;
        }
    }

    return parse();
}

const parsePrefix = (expression) => parseExpression(expression, "prefix");
const parsePostfix = (expression) => parseExpression(expression, "postfix");


// test program
// const testExpr = parsePostfix("(x 2 +)");
// console.log(testExpr.postfix());
// console.log(testExpr.operand);
// console.log(testExpr);
// for (let i = 0; i < 11; i++) {
//     console.log(testExpr.evaluate(i, 0, 0));
// }