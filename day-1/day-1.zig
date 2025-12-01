const std = @import("std");
const fs = std.fs;
const print = std.debug.print;

///  NOTE: get i32 from string (and L => negative)
pub fn parsenum (string: []const u8) i32 {
    var number: i32 = 0;
    for (string[1..]) |c| {
        number = (number * 10) + c - '0';
    }
    number *= if (string[0] == 'L') -1 else 1;
    return number;
}

pub fn main() !void {

    // NOTE: file handling
    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var file_buffer: [4096]u8 = undefined;
    var reader = file.reader(&file_buffer);

    // NOTE: dial and counters
    var dial: i32 = 50;
    var zerocounter_1: u32 = 0;
    var zerocounter_2: u32 = 0;

    // NOTE: main dial counting algorithm
    while (try reader.interface.takeDelimiter('\n')) |line| {

        // count the number of times it touches 0
        const delta: i32 = parsenum(line);
        const full_rots = @abs(@divTrunc(delta, 100));
        const rot = @rem(delta, 100);

        // combined two comparisons using abs for code golf reasons
        // dial + rot > 99 and dial + rot < 1 
        zerocounter_2 += full_rots + @intFromBool(@abs(dial + rot - 50) > 49 and dial != 0); 

        // set new dial position
        dial = @mod(dial + rot, 100);

        // count the number of times it lands on 0
        zerocounter_1 += @intFromBool(dial == 0);
    }
    print("encounterd zeros (Q1): {d}\n", .{zerocounter_1});
    print("encounterd zeros (Q2): {d}\n", .{zerocounter_2});
}
