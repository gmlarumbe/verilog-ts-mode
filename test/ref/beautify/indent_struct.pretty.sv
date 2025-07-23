module foo;

assign a = { g + c };
assign a = c;

typedef struct {
reg 		 r;
ahb_op_t 	 op; 	    // Read, write, etc.
ahb_cycle_type_t cti; 	    // Cycle type for bursts
ahb_incr_type_t  incr; 	    // Increment type (for bursts)
bit 		 b;
reg 		 r;
ahb_thingy 	 a;
bit [31:2] 	 addr; 	    // Starting address
bit [3:0] 	 byte_sel;  // Byte lane select
int 		 len; 	    // Length of transfer
bit [31:0] 	 data[0:7]; // Write data
} ahb_req_t;

struct {
reg   f;
xyzzy b;
}s1;
struct packed {
int a; // ok
}s2;
struct packed signed {
int a; // woops
}s3;
struct packed unsigned {
int a; // woops
}s4;

endmodule // foo

module foo (
input  a,
input  c,
output d
);
always @(a) g;



endmodule // foo


// Local Variables:
// verilog-align-typedef-words: ("ahb_thingy" "xyzzy")
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
