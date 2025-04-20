const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
const ArrayList = std.ArrayList;

const TokenType = enum { number, operator };
const TokenNumber = struct {
    value: i32,
};
const TokenOperator = enum(u8) {
    _,
    pub fn to_binop(self: TokenOperator) ?BinOpType {
        return switch (@intFromEnum(self)) {
            '+' => BinOpType.add,
            '-' => BinOpType.sub,
            '*' => BinOpType.mul,
            '/' => BinOpType.div,
            else => null,
        };
    }
};
const Token = union(TokenType) {
    number: TokenNumber,
    operator: TokenOperator,
};
fn token_num(value: i32) Token {
    return Token{
        .number = TokenNumber{ .value = value },
    };
}
fn token_op(op: u8) Token {
    return Token{
        .operator = @enumFromInt(op),
    };
}

const LexerError = error{
    InvalidToken,
};

const Lexer = struct {
    input: []const u8,
    pos: usize,
    current: ?Token,
    peeked: ?Token,

    pub fn init(input: []const u8) !Lexer {
        return Lexer{
            .input = input,
            .pos = 0,
            .current = null,
            .peeked = null,
        };
    }

    fn advance(self: *Lexer) !?Token {
        assert(self.pos < self.input.len);

        const c = self.input[self.pos];
        self.pos += 1;

        switch (c) {
            '0'...'9' => {
                const t = self.current;
                if (t) |tok| switch (tok) {
                    .number => |n| {
                        self.current = token_num(n.value * 10 + c - '0');
                        return null;
                    },
                    else => {},
                };
                self.current = token_num(c - '0');
                return t;
            },
            '+', '-', '*', '/' => {
                const t = self.current;
                self.current = token_op(c);
                return t;
            },
            ' ' => {
                return null;
            },
            else => {
                return LexerError.InvalidToken;
            },
        }
    }

    pub fn next(self: *Lexer) !?Token {
        if (self.peeked) |t| {
            self.peeked = null;
            return t;
        }
        while (true) {
            if (self.pos < self.input.len) {
                const token = try self.advance();
                if (token) |t| return t;
            } else {
                const t = self.current;
                self.current = null;
                return t;
            }
        }
    }

    pub fn peek(self: *Lexer) !?Token {
        if (self.peeked) |t| return t;
        const token = try self.next();
        if (token) |t| self.peeked = t;
        return token;
    }
};

const NodeType = enum { number, binop };
const Assoc = enum { left, right };
const BinOpType = enum {
    add,
    sub,
    mul,
    div,
    pub fn to_string(self: BinOpType) []const u8 {
        return switch (self) {
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
        };
    }
    pub fn precedence(self: BinOpType) u8 {
        return switch (self) {
            .mul, .div => 2,
            .add, .sub => 1,
        };
    }
    pub fn associativity(self: BinOpType) Assoc {
        return switch (self) {
            .add, .sub, .mul, .div => Assoc.left,
        };
    }
    pub fn binding_power(self: BinOpType) struct { u8, u8 } {
        const prec = self.precedence();
        const assoc = self.associativity();
        return switch (assoc) {
            .left => .{ prec * 2, prec * 2 + 1 },
            .right => .{ prec * 2 + 1, prec * 2 },
        };
    }
};
const BinOp = struct {
    op: BinOpType,
    left: Node.ID,
    right: Node.ID,
};
const Node = union(NodeType) {
    number: i32,
    binop: BinOp,
    pub const ID = enum(u32) { _ };
    pub fn num(value: i32) Node {
        return Node{
            .number = value,
        };
    }
    pub fn bin_op(op: BinOpType, left: ID, right: ID) Node {
        return Node{
            .binop = BinOp{
                .op = op,
                .left = left,
                .right = right,
            },
        };
    }
};
const NodeStore = struct {
    nodes: ArrayList(Node),
    pub fn init(gpa: std.mem.Allocator) NodeStore {
        return NodeStore{
            .nodes = ArrayList(Node).init(gpa),
        };
    }
    pub fn add(self: *NodeStore, node: Node) !Node.ID {
        const next: Node.ID = @enumFromInt(self.nodes.items.len);
        try self.nodes.append(node);
        return next;
    }
    pub fn to_string(self: *NodeStore, id: Node.ID, gpa: std.mem.Allocator) ![]const u8 {
        const node = self.nodes.items[@intFromEnum(id)];
        switch (node) {
            .number => |n| return std.fmt.allocPrint(gpa, "{d}", .{n}),
            .binop => |b| {
                const left_str = try self.to_string(b.left, gpa);
                const right_str = try self.to_string(b.right, gpa);
                return std.fmt.allocPrint(gpa, "({s} {s} {s})", .{ b.op.to_string(), left_str, right_str });
            },
        }
    }
};

const ParserError = error{
    ExpectedNumber,
    ExpectedOperator,
    UnexpectedEOF,
};
fn expr_bp(lexer: *Lexer, node_store: *NodeStore, min_bp: u8) !Node.ID {
    var lhs = if (try lexer.next()) |t| switch (t) {
        .number => |n| try node_store.add(Node.num(n.value)),
        else => return ParserError.ExpectedNumber,
    } else return ParserError.UnexpectedEOF;

    loop: while (true) {
        const op: ?BinOpType = if (try lexer.peek()) |t| switch (t) {
            .operator => |op| op.to_binop(),
            else => null,
        } else break :loop;
        if (op) |binop| {
            const lbp, const rbp = binop.binding_power();
            if (lbp < min_bp) break :loop;

            _ = try lexer.next(); // consume the operator
            const rhs = try expr_bp(lexer, node_store, rbp);
            lhs = try node_store.add(Node.bin_op(binop, lhs, rhs));
        } else return ParserError.ExpectedOperator;
    }
    return lhs;
}

test "tokenize" {
    var lexer = try Lexer.init("32 + 54 - 21");

    const expected = [_]?Token{
        token_num(32),
        token_op('+'),
        token_num(54),
        token_op('-'),
        token_num(21),
        null,
    };
    const got = [_]?Token{
        try lexer.next(),
        try lexer.next(),
        try lexer.next(),
        try lexer.next(),
        try lexer.next(),
        try lexer.next(),
    };
    try std.testing.expectEqual(expected, got);
}

test "trivial parse" {
    const gpa = std.heap.page_allocator;

    var lexer = try Lexer.init("32");
    var node_store = NodeStore.init(gpa);

    const id = try expr_bp(&lexer, &node_store, 0);
    const got = try node_store.to_string(id, gpa);
    try std.testing.expectEqualStrings("32", got);
}

test "parse 1+2*3" {
    const gpa = std.heap.page_allocator;

    var lexer = try Lexer.init("1 + 2 * 3");
    var node_store = NodeStore.init(gpa);

    const id = try expr_bp(&lexer, &node_store, 0);
    const got = try node_store.to_string(id, gpa);
    try std.testing.expectEqualStrings("(+ 1 (* 2 3))", got);
}

test "parse 1+2*3*4+5" {
    const gpa = std.heap.page_allocator;

    var lexer = try Lexer.init("1+2*3*4+5");
    var node_store = NodeStore.init(gpa);

    const id = try expr_bp(&lexer, &node_store, 0);
    const got = try node_store.to_string(id, gpa);
    try std.testing.expectEqualStrings("(+ (+ 1 (* (* 2 3) 4)) 5)", got);
}
