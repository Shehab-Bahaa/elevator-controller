---------------------------------------------------------------------------------------------------------
--                                          Unit_Control                                               --
---------------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

ENTITY Unit_Control IS
    GENERIC ( N : NATURAL := 10; SIMULATION: NATURAL := 0);         -- N: floors count, SIMULATION: 0 for the synthesis one-sec counter 
                                                                    --                  SIMULATION: 1 for the simulation 400ns counter 
    PORT (  clk       : IN STD_LOGIC;
            reset_n   : IN STD_LOGIC;

            req       : IN STD_LOGIC_VECTOR (N downto 1);       -- same size as "floor_status" and "b" input
            
            o_up      : OUT STD_LOGIC;
            o_down    : OUT STD_LOGIC;
            o_open    : OUT STD_LOGIC;
            o_floor   : OUT STD_LOGIC_VECTOR (N downto 1)       -- same size as "b" input
            );
END Unit_Control;

ARCHITECTURE Behavioral OF Unit_Control IS
    COMPONENT counter
        GENERIC ( n_bits : NATURAL := 4 ; k: NATURAL := 16);
        PORT (  clock   : IN STD_LOGIC;
                reset_n : IN STD_LOGIC;        
                En      : IN STD_LOGIC;
                Load    : IN STD_LOGIC;
                data_in : IN STD_LOGIC_VECTOR(n_bits-1 downto 0);
                rollover: OUT STD_LOGIC );
    END COMPONENT;

    TYPE State_type IS (IDLE, GOING_UP, GOING_DOWN, DOOR_OPEN);
    ATTRIBUTE syn_encoding                  : string;
    ATTRIBUTE syn_encoding OF State_type    : TYPE IS "00 01 10 11";
    SIGNAL  state                           : State_type;
    SIGNAL  temp_floor, 
            next_floor                      : STD_LOGIC_VECTOR (N downto 1);
    SIGNAL  counters_reset, 
            Clear_counters, 
            one_sec_rollover, 
            timer_rollover,
            Load                            : STD_LOGIC;
BEGIN
    counters_reset <= reset_n AND NOT (Clear_counters);

    FORSYN: IF SIMULATION = 0 GENERATE
        one_sec: counter GENERIC MAP ( n_bits => 26, k => 50000000)
                PORT MAP ( clock    => clk,
                            reset_n  => counters_reset,        
                            En       => '1',
                            Load     => Load,
                            data_in  => (0 => '1', others => '0'),
                            rollover => one_sec_rollover
                        );
    END GENERATE FORSYN;
    FORSIM: IF SIMULATION = 1 GENERATE
        one_sec: counter GENERIC MAP ( n_bits => 26, k => 20)
                PORT MAP ( clock    => clk,
                            reset_n  => counters_reset,        
                            En       => '1',
                            Load     => Load,
                            data_in  => (0 => '1', others => '0'),
                            rollover => one_sec_rollover
                        );
    END GENERATE FORSIM;  

    timer:  counter GENERIC MAP ( n_bits => 1, k => 2)
            PORT MAP (  clock    => clk,
                        reset_n  => counters_reset,        
                        En       => one_sec_rollover,
                        Load     => '0',
                        data_in  => (others => '0'),
                        rollover => timer_rollover
                     );

    process (clk, reset_n) -- state flip-flops
    begin
        IF (reset_n = '0') THEN     
            state           <= IDLE;
            o_up            <= '0';
            o_down          <= '0';
            o_open          <= '0';
            Clear_counters  <= '0';
            Load            <='0';
            next_floor      <= (N downto 2 => '0') & '1';
        ELSIF rising_edge(clk) THEN 
            case state IS
                WHEN IDLE =>
                    IF (req = (N downto 1 => '0')) THEN 
                        state <= IDLE;
                        o_up            <= '0';
                        o_down          <= '0';
                        o_open          <= '0';
                        Clear_counters  <= '0';
                        Load            <= '0';
                    ELSIF (req > temp_floor) THEN
                        state           <= GOING_UP;
                        o_up            <= '1';
                        o_down          <= '0';
                        o_open          <= '0';
                        Clear_counters  <= '1';
                        Load            <= '1';
                    ELSIF (req = temp_floor) THEN
                        state           <= DOOR_OPEN;
                        o_up            <= '0';
                        o_down          <= '0';
                        o_open          <= '1';
                        Clear_counters  <= '1';
                        Load            <= '1';
                    ELSE
                        state           <= GOING_DOWN;
                        o_up            <= '0';
                        o_down          <= '1';
                        o_open          <= '0';
                        Clear_counters  <= '1';
                        Load            <= '1'; 
                    END IF;
                
                WHEN GOING_UP =>
                    Clear_counters  <= '0';
                    Load            <= '0';
                    IF (one_sec_rollover = '1' AND timer_rollover = '1') THEN 
                        IF (req = temp_floor (N -1 downto 1) & '0') THEN 
                            state           <= DOOR_OPEN;
                            o_up            <= '0';
                            o_down          <= '0';
                            o_open          <= '1';
                            Clear_counters  <= '1';
                            Load            <= '1';
                        ELSE 
                            state           <= GOING_UP;
                            o_up            <= '1';
                            o_down          <= '0';
                            o_open          <= '0';
                            Clear_counters  <= '1';
                            Load            <= '1';
                        END IF;
                        next_floor          <= temp_floor (N -1 downto 1) & '0' ;
                    ELSE 
                        state               <= GOING_UP;
                    END IF;

                WHEN GOING_DOWN =>
                    Clear_counters  <= '0';
                    Load            <= '0';
                    IF (one_sec_rollover = '1' AND timer_rollover = '1') THEN 
                        IF (req = '0' & temp_floor (N downto 2)) THEN 
                            state           <= DOOR_OPEN;
                            o_up            <= '0';
                            o_down          <= '0';
                            o_open          <= '1';
                            Clear_counters  <= '1';
                            Load            <= '1';
                        ELSE 
                            state           <= GOING_DOWN;
                            o_up            <= '0';
                            o_down          <= '1';
                            o_open          <= '0';
                            Clear_counters  <= '1';
                            Load            <= '1';
                        END IF;
                        next_floor <= '0' & temp_floor (N downto 2);
                    ELSE 
                        state <= GOING_DOWN;
                    END IF;
                    
                WHEN DOOR_OPEN =>
                    Clear_counters  <= '0';
                    Load            <= '0';
                    IF (one_sec_rollover = '1' AND timer_rollover = '1') THEN 
                        IF (req = (N downto 1 => '0')) THEN 
                            state           <= IDLE;
                            o_up            <= '0';
                            o_down          <= '0';
                            o_open          <= '0';
                            Clear_counters  <= '0';
                            Load            <= '0';
                        ELSIF (req > temp_floor) THEN
                            state           <= GOING_UP;
                            o_up            <= '1';
                            o_down          <= '0';
                            o_open          <= '0';
                            Clear_counters  <= '1';
                            Load            <= '1';
                        ELSE
                            state           <= GOING_DOWN;
                            o_up            <= '0';
                            o_down          <= '1';
                            o_open          <= '0';
                            Clear_counters  <= '1';
                            Load            <= '1'; 
                        END IF;
                    ELSE 
                        state               <= DOOR_OPEN;
                    END IF;

            END CASE;
        END IF;
    end process; -- state flip-flops

    temp_floor <= next_floor;
    o_floor <= temp_floor;
    
END Behavioral;
