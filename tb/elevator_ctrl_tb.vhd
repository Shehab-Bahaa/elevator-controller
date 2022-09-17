---------------------------------------------------------------------------------------------------------
--                                              TESTBENCH                                              --
---------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE std.textio.all;
USE std.env.finish;
USE work.tb_subprograms.ALL;

entity elevator_ctrl_tb is
    GENERIC ( N : NATURAL := 10);
end entity elevator_ctrl_tb;

architecture test of elevator_ctrl_tb is
    COMPONENT elevator_ctrl
        GENERIC ( N : NATURAL := 10; SIMULATION: NATURAL := 0);       -- floors count 
        PORT (  clk       : IN STD_LOGIC;
                reset_n   : IN STD_LOGIC;

                b         : IN STD_LOGIC_VECTOR (N downto 1);       -- buttons inside elevator
                up        : IN STD_LOGIC_VECTOR (N-1 downto 1);     -- up buttons on each floor
                dn        : IN STD_LOGIC_VECTOR (N downto 2);       -- down buttons on each floor

                o_up      : OUT STD_LOGIC;
                o_down    : OUT STD_LOGIC;
                o_open    : OUT STD_LOGIC;
                o_floor   : OUT STD_LOGIC_VECTOR (N downto 1);       -- same size as "b" input

                seg       : OUT std_logic_vector(6 downto 0)
            );
    END COMPONENT;

    SIGNAL clk: std_logic := '0';
    SIGNAL reset_n: std_logic := '0';

    SIGNAL b         : STD_LOGIC_VECTOR (N downto 1);
    SIGNAL up        : STD_LOGIC_VECTOR (N-1 downto 1);
    SIGNAL dn        : STD_LOGIC_VECTOR (N downto 2);
    
    SIGNAL o_up      : STD_LOGIC;
    SIGNAL o_down    : STD_LOGIC;
    SIGNAL o_open    : STD_LOGIC;
    SIGNAL o_floor   : STD_LOGIC_VECTOR (N downto 1);
    SIGNAL seg       : STD_LOGIC_VECTOR (6 downto 0);

begin
    -- Instantiate the design under test
    dut: elevator_ctrl  GENERIC map ( N => N, SIMULATION => 1)
                        port map (   clk     => clk,
                                    reset_n => reset_n,

                                    b       => b,
                                    up      => up,
                                    dn      => dn,
                                    
                                    o_up    => o_up,
                                    o_down  => o_down,
                                    o_open  => o_open,
                                    o_floor => o_floor,
                                    seg     => seg
                                    );
    -- reset_n and clock
    clk <= not clk after 10 ns;
    reset_n <= '0', '1' after 40 ns;
    
    -- Generator
    PROCESS 
        file     data1, 
                 data2              : text;
        variable sample, 
                 write_out          : line;
        -- variables for the input file
        variable b_var                : STD_LOGIC_VECTOR (N downto 1);
        variable up_var               : STD_LOGIC_VECTOR (N-1 downto 1);
        variable dn_var               : STD_LOGIC_VECTOR (N downto 2);
        variable Expected_final_floor : INTEGER;
        variable OK                   : boolean;
        variable o_floor_var          : STD_LOGIC_VECTOR (N downto 1);
        variable o_up_var             : STD_LOGIC;
        variable o_down_var           : STD_LOGIC;
        variable o_open_var           : STD_LOGIC;
        variable state_start_time     : time := 0 ns;
        variable state_end_time       : time := 0 ns;
        variable state_duration       : time;
        variable previous_state       : string (1 to 10);
        variable previous_floor       : STD_LOGIC_VECTOR (N downto 1);
    BEGIN
        wait until (reset_n = '1');
        WAIT FOR 200 ns;
        --report (to_string(now, ns) & "1_Debugging" & LF); 
        file_open(data1, "stimuli_and_expected_outputs.txt", read_mode);
        file_open (data2, "SIMULATION_RESULTS.txt", write_mode);

        write (write_out, string'("# NOTE: to reduce the simulation time, the period of the one_sec timer is modified to 400 ns instead of 1 sec ONLY during simulation (handled automatically using if-generate)"));
        report ("---------> to reduce the simulation time, the period of the one_sec timer is modified to 400 ns instead of 1 sec ONLY during simulation (handled automatically using if-generate)" & LF );        
        writeline (data2, write_out);
        
        WHILE NOT endfile (data1) LOOP
            readline (data1, sample);
            read (sample, b_var, OK);
            --report (to_string(now, ns) & to_string(OK) & LF);
            if OK then 
                read (sample, up_var);
                read (sample, dn_var);
                read (sample, Expected_final_floor);
                b  <= b_var;
                up <= up_var;
                dn <= dn_var;
                WAIT FOR 0 ns;
                --report (to_string(now, ns) & "2_Debugging" & LF); 
                WAIT FOR 20 ns;
                b  <= (others => '1');
                up <= (others => '1');
                dn <= (others => '1');
                WAIT FOR 0 ns;
                --report (to_string(now, ns) & "3_Debugging" & LF); 
                write (write_out, string'("-----------------------------------------TEST START-----------------------------------------"));
                writeline (data2, write_out);

                state_start_time    := 0 ns;
                state_end_time      := 0 ns;
                LOOP
                --report (to_string(now, ns) & "4_Debugging" & LF); 
                    WAIT UNTIL (o_floor'event OR o_open'event OR o_up'event OR o_down'event);
                    --report (to_string(now, ns) & "5_Debugging" & LF);
                    
                    IF NOT (state_start_time = 0 ns) THEN
                        state_end_time := NOW;
                        state_duration := state_end_time - state_start_time;

                        IF NOT (state_duration = 800 ns) THEN
                            write (write_out, string'("TEST FAILED: incorrect state duration"));
                            writeline (data2, write_out);
                            write (write_out, string'("SUGGESTION: Check that the k generic of 'one_sec' instance of 'counter' module is 20 (k => 20)"));
                            writeline (data2, write_out);
                        END IF;

                        ASSERT state_duration = 800 ns REPORT "TEST FAILED: incorrect state duration"
                            SEVERITY error;

                        write (write_out, string'("             SUCCESSFUL ") & previous_state & string'(" with ") & time'IMAGE(state_duration) & string'(" duration at floor ") & to_string(OneHot_to_Decimal(previous_floor)));
                        writeline (data2, write_out);
                    END IF;
                    state_start_time := NOW;

                    IF (o_open = '1' AND o_up = '0' AND o_down = '0') THEN
                        IF (o_up'event OR o_down'event) THEN
                            WAIT UNTIL (o_floor'event);
                        END IF;
                        previous_state := "DOOR_OPEN ";
                        write (write_out, string'("at ") & time'IMAGE(now) & string'(": current state is DOOR_OPEN  at floor ") & to_string(OneHot_to_Decimal(o_floor)));
                        writeline (data2, write_out);
                    ELSIF (o_open = '0' AND o_up = '1' AND o_down = '0') THEN
                        previous_state := "GOING_UP  ";
                        write (write_out, string'("at ") & time'IMAGE(now) & string'(": current state is GOING_UP   at floor ") & to_string(OneHot_to_Decimal(o_floor)));
                        writeline (data2, write_out);
                    ELSIF (o_open = '0' AND o_up = '0' AND o_down = '1') THEN
                        previous_state := "GOING_DOWN";
                        write (write_out, string'("at ") & time'IMAGE(now) & string'(": current state is GOING_DOWN at floor ") & to_string(OneHot_to_Decimal(o_floor)));
                        writeline (data2, write_out);
                    END IF;

                    previous_floor := o_floor;

                    IF (NOT (o_floor'event) AND o_open'event AND o_open = '0' AND o_up = '0' AND o_down = '0') THEN     -- idle state (flag that the testcase ended)
                        --report (to_string(now, ns) & "6_Debugging" & LF);

                        IF NOT (o_floor = Decimal_to_OneHot(Expected_final_floor, 10)) THEN
                            write (write_out, string'("TEST FAILED: incorrect final floor"));
                            writeline (data2, write_out);
                        END IF;
                        ASSERT (o_floor = Decimal_to_OneHot(Expected_final_floor, 10)) REPORT "TEST FAILED: incorrect final floor"
                            SEVERITY error;

                        write (write_out, string'("at ") & time'IMAGE(now) & string'(": current state is IDLE       at floor ") & to_string(OneHot_to_Decimal(o_floor)) & LF);
                        write (write_out, string'("                                     TESTCASE SUCCESSFUL") & LF);
                        write (write_out, string'("------------------------------------------TEST END------------------------------------------"));
                        writeline (data2, write_out);
                        EXIT;
                    END IF;
                END LOOP;
                --report (to_string(now, ns) & "7_Debugging" & LF);
            end if;
        END LOOP;
        file_close (data1);
        file_close (data2);
        WAIT FOR 200 ns;
        finish;
        
        -- Testing complete
        wait;
    END PROCESS;

end test;

-- end of file
