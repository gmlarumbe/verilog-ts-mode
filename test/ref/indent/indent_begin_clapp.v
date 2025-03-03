// bug 825
module x;

    always @*
        begin
        end

    initial
        begin
        end

    final
        begin
        end

    initial forever
        begin
        end

    initial begin
        foreach(x[i])
            begin
            end

        do
            begin
            end while (i);
    end

    initial @a.b
        begin
        end

    always @E
        begin
        end

    initial
        forever @E
            begin
            end

endmodule

// Local Variables:
// verilog-indent-begin-after-if: nil
// End:
