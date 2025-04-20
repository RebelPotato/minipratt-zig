const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
const ArrayList = std.ArrayList;

const TokenType = enum { atom, operator };
const Token = union(TokenType) {
    atom: u8,
    operator: u8,
};
fn token_atom(value: u8) Token {
    return Token{ .atom = value };
}
fn token_op(op: u8) Token {
    return Token{ .operator = op };
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
                self.current = token_atom(c - '0');
                return t;
            },
            '+', '-', '*', '/' => {
                const t = self.current;
                self.current = token_op(c);
                return t;
            },
            ' ' => return null,
            else => return LexerError.InvalidToken,
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

const NodeType = enum { atom, binop };
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
pub fn to_binop_type(self: u8) ?BinOpType {
    return switch (self) {
        '+' => BinOpType.add,
        '-' => BinOpType.sub,
        '*' => BinOpType.mul,
        '/' => BinOpType.div,
        else => null,
    };
}
const BinOp = struct {
    op: BinOpType,
    left: Node.ID,
    right: Node.ID,
};
const Node = union(NodeType) {
    atom: i32,
    binop: BinOp,
    pub const ID = enum(u32) { _ };
};
pub fn node_binop(op: BinOpType, left: Node.ID, right: Node.ID) Node {
    return Node{
        .binop = BinOp{
            .op = op,
            .left = left,
            .right = right,
        },
    };
}
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
            .atom => |n| return std.fmt.allocPrint(gpa, "{d}", .{n}),
            .binop => |b| {
                const left_str = try self.to_string(b.left, gpa);
                const right_str = try self.to_string(b.right, gpa);
                return std.fmt.allocPrint(gpa, "({s} {s} {s})", .{ b.op.to_string(), left_str, right_str });
            },
        }
    }
};

const ParserError = error{
    ExpectedAtom,
    ExpectedOperator,
    UnexpectedEOF,
};
fn expr_bp(lexer: *Lexer, node_store: *NodeStore, min_bp: u8) !Node.ID {
    var lhs = if (try lexer.next()) |t| switch (t) {
        .atom => |n| try node_store.add(Node{ .atom = n }),
        else => return ParserError.ExpectedAtom,
    } else return ParserError.UnexpectedEOF;

    loop: while (true) {
        const op_or: ?BinOpType = if (try lexer.peek()) |t| switch (t) {
            .operator => |op| to_binop_type(op),
            else => null,
        } else break :loop;
        if (op_or) |op| {
            const lbp, const rbp = op.binding_power();
            if (lbp < min_bp) break :loop;

            _ = try lexer.next(); // consume the operator
            const rhs = try expr_bp(lexer, node_store, rbp);
            lhs = try node_store.add(node_binop(op, lhs, rhs));
        } else return ParserError.ExpectedOperator;
    }
    return lhs;
}

test "tokenize" {
    var lexer = try Lexer.init("3 + 5 - 2");

    const expected = [_]?Token{
        token_atom(3),
        token_op('+'),
        token_atom(5),
        token_op('-'),
        token_atom(2),
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

    var lexer = try Lexer.init("7");
    var node_store = NodeStore.init(gpa);

    const id = try expr_bp(&lexer, &node_store, 0);
    const got = try node_store.to_string(id, gpa);
    try std.testing.expectEqualStrings("7", got);
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
