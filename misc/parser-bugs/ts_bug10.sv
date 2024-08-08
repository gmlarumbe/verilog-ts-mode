// Update to tree-sitter 22 yielded errors on detection of second task

class foo;
    /// Handle Write response checking, update golden model
    protected task automatic handle_write_resp(input axi_id_t id);
        ax_beat_t  aw_beat;
        b_beat_t   b_beat;
        axi_addr_t bus_address;
        forever begin
            wait (this.b_sample[id].size() > 0);
            assert (this.b_queue[id].size() > 0) else
                wait (this.b_queue[id].size() > 0);
            aw_beat = b_queue[id].pop_front();
            b_beat  = b_sample[id].pop_front();
            if (check_en[BRespCheck]) begin
                assert (b_beat.b_id   == id);
            end
            // pop all accessed memory locations by this beat
            for (int unsigned i = 0; i <= aw_beat.ax_len; i++) begin
                bus_address = axi_pkg::aligned_addr(
                    axi_pkg::beat_addr(aw_beat.ax_addr, aw_beat.ax_size, aw_beat.ax_len, aw_beat.ax_burst,
                    i), BUS_SIZE);
                for (int j = 0; j < axi_pkg::num_bytes(BUS_SIZE); j++) begin
                    if (b_beat.b_resp inside {axi_pkg::RESP_OKAY, axi_pkg::RESP_EXOKAY}) begin
                        memory_q[bus_address+j].delete(0);
                    end else begin
                        memory_q[bus_address+j].delete(memory_q[bus_address+j].size() - 1);
                    end
                end
            end
        end
    endtask : handle_write_resp

    /// Handle read checking against the golden model
    protected task automatic handle_read(input axi_id_t id);
        ax_beat_t  ar_beat;
        r_beat_t   r_beat;
        axi_addr_t bus_address, beat_address, idx_data;
        byte_t     act_data;
        byte_t     exp_data[$];
        byte_t     tst_data[$];
        forever begin
            wait (this.ar_sample[id].size() > 0);
            ar_beat = this.ar_sample[id].pop_front();
            // This scoreborad only supports this type of burst:
            assert (ar_beat.ax_burst == axi_pkg::BURST_INCR || ar_beat.ax_len == '0) else
                $warning("Not supported AR burst: BURST: %0h.", ar_beat.ax_burst);

            for (int unsigned i = 0; i <= ar_beat.ax_len; i++) begin
                wait (this.r_sample[id].size() > 0);
                r_beat = this.r_sample[id].pop_front();
                beat_address = axi_pkg::beat_addr(ar_beat.ax_addr, ar_beat.ax_size, ar_beat.ax_len,
                ar_beat.ax_burst, i);
                beat_address = axi_pkg::aligned_addr(beat_address, ar_beat.ax_size);
                bus_address  = axi_pkg::aligned_addr(beat_address, BUS_SIZE);
                if (!this.memory_q.exists(bus_address)) begin
                    for (int unsigned j = 0; j < axi_pkg::num_bytes(BUS_SIZE); j++) begin
                        this.memory_q[bus_address+j].push_back(8'bxxxxxxxx);
                    end
                end
                // Assert that the correct data is read.
                if (this.check_en[ReadCheck] &&
                (r_beat.r_resp inside {axi_pkg::RESP_OKAY, axi_pkg::RESP_EXOKAY})) begin
                    for (int unsigned j = 0; j < axi_pkg::num_bytes(ar_beat.ax_size); j++) begin
                        idx_data  = 8*BUS_SIZE'(beat_address+j);
                        act_data  = r_beat.r_data[idx_data+:8];
                        exp_data  = this.memory_q[beat_address+j];
                        if (exp_data.size() > 0) begin
                            tst_data  = exp_data.find with (item === 8'hxx || item === act_data);
                            assert (tst_data.size() > 0) else begin
                                $warning("Unexpected RData ID: %0h \n \
                                    Addr:     %h \n \
                                    Byte Idx: %h \n \
                                    Exp Data: %h \n \
                                    Act Data: %h \n \
                                    BeatData: %h",
                                r_beat.r_id, beat_address+j, idx_data, exp_data, act_data, r_beat.r_data);
                            end
                        end
                    end
                end
            end
            if (this.check_en[RRespCheck]) begin
                assert (r_beat.r_id   == id);
                assert (r_beat.r_last);
            end
        end
    endtask : handle_read
endclass
