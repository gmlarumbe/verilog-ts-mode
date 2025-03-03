module t;
    default clocking @(posedge clk);
    endclocking
    default clocking clocking_identifier;
    property foo (a);
        a |-> b ##2 c;
    endproperty
    cover property (prop) $display("**COVERAGE**");
    assert property (foo) a;
    assume property (bar) b;
    b1: assume property (bar) b;
    B2: assert property (foo) a;
    B2: cover property (foo) a;
    assume property (bar) b;
    // a;
endmodule
