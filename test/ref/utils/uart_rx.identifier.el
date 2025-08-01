(("FREQ_CLK" "parameter_declaration")
 ("TX_SPEED" "parameter_declaration")
 ("Clk" "input logic")
 ("Rst_n" "input logic")
 ("RXD" "input logic")
 ("RX_Valid" "output logic")
 ("RX_Load" "output logic")
 ("PULSE_END_OF_COUNT" "local_parameter_declaration")
 ("PULSE_END_OF_COUNT_HALF" "local_parameter_declaration")
 ("state_t" "enum logic[2:0] {\n                 IDLE = 0,\n                 START_BIT = 1,\n                 HALF_BIT_DLY = 2,\n                 RCV_DATA = 3,\n                 STOP_BIT = 4\n                 }")
 ("state" "state_t")
 ("next_state" "state_t")
 ("period_cnt" "logic [31:0]")
 ("period_ctr_ena" "logic")
 ("bit_cnt" "logic [31:0]")
 ("bit_ctr_ena" "logic")
 ("bit_end" "logic")
 ("half_bit_cnt" "logic [31:0]")
 ("half_bit_ctr_ena" "logic")
 ("half_bit_rstn" "logic")
 ("data_rcv_end" "logic")
 ("uart_rx" "module_declaration"))
