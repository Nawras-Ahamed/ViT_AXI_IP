module sysArr #(parameter COL = 16, bit_width = 8, acc_width = 16 ,data_width = 8 * COL,
   weight_width = 8 * COL, sum_width = 16 * COL)
(
    input clk,
    input active,

    input [data_width-1:0] datain, // 8 bits for each row Topmost row has LSB
    input [weight_width-1:0] win, // 8 bits for each column Leftmost column has LSB
    input [sum_width-1:0] sumin,
    input [COL-1:0] wwrite, 

    // Outputs from bottom row of array
    output wire [sum_width-1:0] maccout, 
    output wire [COL-1:0] wwriteout,
    output wire [COL-1:0] activeout,

    output [data_width-1:0] dataout 
);

    wire [weight_width-1:0] wout;

    //(Row - Row Connections)
    wire [((COL-1)*COL*acc_width)-1:0] mac_out_internal; 
    wire [((COL-1)*COL*bit_width)-1:0] weight_out_internal;
    wire [((COL-1)*COL)-1:0] wwrite_out_internal;
    wire [((COL-1)*COL)-1:0] active_out_internal;

    genvar i;
    generate
        for (i = 0; i < COL; i = i + 1) begin 
            if (i == 0) begin
               
                sysArrRow first_sysArrRow_inst(
                    .clk      (clk),
                    .active   (active),
                    .datain   (datain[((i+1)*bit_width)-1:(i*bit_width)]),
                    .win      (win),
                    .sumin    (256'd0),
                    .wwrite   (wwrite),
                    .maccout  (mac_out_internal[((i+1)*COL*acc_width)-1:(i*COL*acc_width)]),
                    .wout     (weight_out_internal[((i+1)*COL*bit_width)-1:(i*COL*bit_width)]),
                    .wwriteout(wwrite_out_internal[((i+1)*COL)-1:(i*COL)]),
                    .activeout(active_out_internal[((i+1)*COL)-1:(i*COL)]),
                    .dataout  (dataout[((i+1)*bit_width)-1:(i*bit_width)])
                );

            end 

            else if (i == COL-1) begin
                
                sysArrRow last_sysArrRow_inst(
                    .clk      (clk),
                    .active   (active_out_internal[((i-1)*COL)]),
                    .datain   (datain[((i+1)*bit_width)-1:(i*bit_width)]),
                    .win      (weight_out_internal[(i*COL*bit_width)-1:((i-1)*COL*bit_width)]),
                    .sumin    (mac_out_internal[(i*COL*acc_width)-1:((i-1)*COL*acc_width)]),
                    .wwrite   (wwrite_out_internal[(i*COL)-1:((i-1)*COL)]),
                    .maccout  (maccout),
                    .wout     (wout),
                    .wwriteout(wwriteout),
                    .activeout(activeout),
                    .dataout  (dataout[((i+1)*8)-1:(i*8)])
                );

            end 

            else begin
                sysArrRow sysArrRow_inst(
                    .clk      (clk),
                    .active   (active_out_internal[((i-1)*COL)]),
                    .datain   (datain[((i+1)*bit_width)-1:(i*bit_width)]),
                    .win      (weight_out_internal[(i*COL*bit_width)-1:((i-1)*COL*bit_width)]),
                    .sumin    (mac_out_internal[(i*COL*acc_width)-1:((i-1)*COL*acc_width)]),
                    .wwrite   (wwrite_out_internal[(i*COL)-1:((i-1)*COL)]),
                    .maccout  (mac_out_internal[((i+1)*COL*acc_width)-1:(i*COL*acc_width)]),
                    .wout     (weight_out_internal[((i+1)*COL*bit_width)-1:(i*COL*bit_width)]),
                    .wwriteout(wwrite_out_internal[((i+1)*COL)-1:(i*COL)]),
                    .activeout(active_out_internal[((i+1)*COL)-1:(i*COL)]),
                    .dataout  (dataout[((i+1)*bit_width)-1:(i*bit_width)])
                );
            end 
        end 
    endgenerate

endmodule

module sysArrRow #(parameter ROW = 16 ,bit_width = 8, acc_width = 16,
                   weight_width = bit_width * ROW,sum_width = acc_width * ROW)
(
      input clk,
      input active,
      input[7:0] datain,

      input [weight_width-1:0] win, // 8 bits for each PE. Left most PE has LSB
      input [sum_width-1:0] sumin, // 16 bits for each PE. Left most PE has LSB
      input [ROW-1:0] wwrite, 
    
      output wire [sum_width-1:0] maccout,
      output wire [weight_width-1:0] wout,
      output wire [ROW-1:0] wwriteout,
      output wire [ROW-1:0] activeout,
      output [7:0] dataout
);

   

    
    // (PE - PE Connections)
    wire [ROW-1:0] activeout_inter;
    wire [(weight_width-8)-1:0] dataout_inter;

    assign activeout = activeout_inter;

    genvar i;

    generate
        for (i = 0; i < ROW; i = i + 1) begin 
            if (i == 0) begin
                MAC first_MAC_inst(
                    .clk(clk),
                    .active(active),
                    .datain(datain),
                    .win(win[(bit_width-1):0]),
                    .sumin(sumin[(acc_width-1):0]),
                    .wwrite(wwrite[0]),
                    .maccout(maccout[(acc_width-1):0]),
                    .dataout(dataout_inter[7:0]),
                  .wout(wout[(bit_width-1):0]),
                    .wwriteout(wwriteout[0]),
                    .activeout(activeout_inter[i])
                );
            end 

            else if (i == ROW - 1) 
            begin
                MAC last_MAC_inst(
                    .clk(clk),
                    .active(activeout_inter[i-1]),

                    .datain(dataout_inter[(i*bit_width)-1:(i-1)*bit_width]),
                    .win(win[((i+1)*bit_width)-1:(i*bit_width)]),
                    .sumin(sumin[((i+1)*acc_width)-1:(i*acc_width)]),

                    .wwrite(wwrite[ROW-1]),

                    .maccout(maccout[((i+1)*acc_width)-1:(i*acc_width)]),

                    .dataout(dataout),
                    .wout(wout[((i+1)*bit_width)-1:(i*bit_width)]),

                    .wwriteout(wwriteout[ROW-1]),
                    .activeout(activeout_inter[i])
                );
            end 
            else begin
                MAC MAC_inst(
                    .clk(clk),
                    .active(activeout_inter[i-1]),

                    .datain(dataout_inter[(i*bit_width)-1:(i-1)*bit_width]),
                    .win(win[((i+1)*bit_width)-1:(i*bit_width)]),
                    .sumin(sumin[((i+1)*acc_width)-1:(i*acc_width)]),

                    .wwrite(wwrite[i]),

                    .maccout(maccout[((i+1)*acc_width)-1:(i*acc_width)]),

                    .dataout(dataout_inter[((i+1)*bit_width)-1:(i*bit_width)]),
                    .wout(wout[((i+1)*bit_width)-1:(i*bit_width)]),
                    
                    .wwriteout(wwriteout[i]),
                    .activeout(activeout_inter[i])
                );
            end
        end 
    endgenerate
endmodule 

module MAC #(
    parameter bit_width = 8,
    parameter acc_width = 16)
  (
    input clk,
    input active,
    input wwrite,
    input [acc_width - 1:0] sumin,
    input [bit_width - 1:0] datain,
    input [bit_width - 1:0] win,

    output reg [acc_width - 1:0] maccout,
    output reg [bit_width - 1:0] dataout,
    output reg [bit_width - 1:0] wout,
    output reg wwriteout,
    output reg activeout
);
   
    //internal register for the combinational block and pipelining

    reg wwrite_out_comb, active_out_comb;
    reg [acc_width- 1:0] mac_out_comb;
    reg [bit_width - 1:0] data_out_c,wout_c;
    reg [bit_width - 1:0] weight, weight_c;

    wire [acc_width-1:0] mult_result;

    multiply_gen m(
        .A(datain),
        .B(weight),
        .result(mult_result)
    );

    //pipelined with the comb signals
    always @ (posedge clk) 
    begin
            wwriteout <= wwrite_out_comb;
            activeout <= active_out_comb;
            maccout <= mac_out_comb;
            dataout <= data_out_c;
            weight <= weight_c;
            wout <= win;
    end
    // MAC Block
    always @(*) 
    begin
        active_out_comb = active;
        if (active == 1)
        begin
            data_out_c = datain;
            mac_out_comb = sumin + mult_result;
        end

        else  //Stall the pipeline latching the data
        begin
            data_out_c = dataout;
            mac_out_comb = maccout;
        end
    end

    always @(*)
    begin
        wwrite_out_comb = wwrite;

        if ((wwrite ==1) || (wwriteout == 1))
        begin
            weight_c = win;
            wout_c = weight;
        end

        else
        begin
            wout_c = 8'h00;
            weight_c = weight;
        end
    end

endmodule

//Can also initialize the DSP of kintex Gensys 2 FPGA but we are going with LUT based design
module multiply_gen #(
    parameter bit_width = 8,
    parameter acc_width = 16)
    (
    input [bit_width - 1:0] A,B,
    output reg [acc_width - 1:0] result
    );
    always @(A,B)
        result = A * B;
endmodule

