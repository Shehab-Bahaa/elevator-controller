---------------------------------------------------------------------------------------------------------
--                                          elevator_ctrl                                              --
---------------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

 
ENTITY elevator_ctrl IS
    GENERIC ( N : NATURAL := 10; SIMULATION: NATURAL := 0);         -- N: floors count, SIMULATION: 0 for the synthesis one-sec counter 
                                                                    --                  SIMULATION: 1 for the simulation 400ns counter
    PORT (  clk       : IN STD_LOGIC;
            reset_n   : IN STD_LOGIC;

            b         : IN STD_LOGIC_VECTOR (N downto 1);       -- Active-low buttons inside elevator
            up        : IN STD_LOGIC_VECTOR (N-1 downto 1);     -- Active-low up buttons on each floor
            dn        : IN STD_LOGIC_VECTOR (N downto 2);       -- Active-low down buttons on each floor
            
            o_up      : OUT STD_LOGIC;
            o_down    : OUT STD_LOGIC;
            o_open    : OUT STD_LOGIC;
            o_floor   : OUT STD_LOGIC_VECTOR (N downto 1);       -- same size as "b" input
            
            seg       : OUT std_logic_vector(6 downto 0)
            );
END elevator_ctrl;

ARCHITECTURE STRUCTURAL OF elevator_ctrl IS
    COMPONENT Request_Resolver
        GENERIC ( N : NATURAL := 10);       -- floors count 
        PORT (  clk            : IN STD_LOGIC;
                reset_n        : IN STD_LOGIC;

                b              : IN STD_LOGIC_VECTOR (N downto 1);
                up_request     : IN STD_LOGIC_VECTOR (N-1 downto 1);
                dn_request     : IN STD_LOGIC_VECTOR (N downto 2);
                
                up_status      : IN STD_LOGIC;
                down_status    : IN STD_LOGIC;
                open_status    : IN STD_LOGIC;
                floor_status   : IN STD_LOGIC_VECTOR (N downto 1);

                req            : OUT STD_LOGIC_VECTOR (N downto 1)
                );
    END COMPONENT;

    COMPONENT Unit_Control 
        GENERIC ( N : NATURAL := 10; SIMULATION: NATURAL := 0);       -- floors count 
        PORT (  clk       : IN STD_LOGIC;
                reset_n   : IN STD_LOGIC;

                req       : IN STD_LOGIC_VECTOR (N downto 1);
                
                o_up      : OUT STD_LOGIC;
                o_down    : OUT STD_LOGIC;
                o_open    : OUT STD_LOGIC;
                o_floor   : OUT STD_LOGIC_VECTOR (N downto 1)
                );
    END COMPONENT;
    COMPONENT ssd   -- seven-segment display decoder
        PORT (
            one_hot_number  : IN std_logic_vector(15 downto 1);
            seg             : OUT std_logic_vector(6 downto 0)
        );
    END COMPONENT;

    SIGNAL NOT_b            : STD_LOGIC_VECTOR (N downto 1);  
    SIGNAL NOT_up           : STD_LOGIC_VECTOR (N-1 downto 1);
    SIGNAL NOT_dn           : STD_LOGIC_VECTOR (N downto 2);  

    SIGNAL up_tmp           : STD_LOGIC;
    SIGNAL down_tmp         : STD_LOGIC;
    SIGNAL open_tmp         : STD_LOGIC;
    SIGNAL floor_tmp        : STD_LOGIC_VECTOR (N downto 1);
    SIGNAL req_tmp          : STD_LOGIC_VECTOR (N downto 1);

    SIGNAL one_hot_number   : std_logic_vector(15 downto 1);
BEGIN



    FORSYN: IF SIMULATION = 0 GENERATE
        NOT_b   <= (N downto 5 => '0') & NOT(b(4 downto 1));
        NOT_up  <= (others => '0');
        NOT_dn  <= (others => '0');
    END GENERATE FORSYN;
    FORSIM: IF SIMULATION = 1 GENERATE
        NOT_b   <= NOT(b);
        NOT_up  <= NOT(up);
        NOT_dn  <= NOT(dn);
    END GENERATE FORSIM;  

    o_up    <= up_tmp;
    o_down  <= down_tmp;
    o_open  <= open_tmp;
    o_floor <= floor_tmp;

    Request_Resolver_inst   : Request_Resolver GENERIC map (N => N)   
                              port map (  clk            => clk,
                                          reset_n        => reset_n,
  
                                          b              => NOT_b,
                                          up_request     => NOT_up,
                                          dn_request     => NOT_dn,
                                          
                                          up_status      => up_tmp,
                                          down_status    => down_tmp,
                                          open_status    => open_tmp,
                                          floor_status   => floor_tmp,
  
                                          req            => req_tmp
                                        ); 
    Unit_Control_inst       : Unit_Control GENERIC map ( N => N, SIMULATION => SIMULATION)
                              port map (  clk       => clk,
                                          reset_n   => reset_n,
                          
                                          req       => req_tmp,
                                          
                                          o_up      => up_tmp,
                                          o_down    => down_tmp,
                                          o_open    => open_tmp,
                                          o_floor   => floor_tmp
                                        );
    
    one_hot_number <= (15 downto N +1 => '0') & floor_tmp;
    ssd_inst                : ssd
                              PORT MAP (
                                          one_hot_number  => one_hot_number,
                                          seg             => seg
                                        );

END STRUCTURAL;
---------------------------------------------------------------------------------------------------------
--                                          Request_Resolver                                           --
---------------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL; USE ieee.numeric_std.ALL; USE IEEE.math_real.ALL;


ENTITY Request_Resolver IS
    GENERIC ( N : NATURAL := 10);       -- floors count 
    PORT (  clk            : IN STD_LOGIC;
            reset_n        : IN STD_LOGIC;

            b              : IN STD_LOGIC_VECTOR (N downto 1);       -- buttons inside elevator
            up_request     : IN STD_LOGIC_VECTOR (N-1 downto 1);     -- up buttons on each floor
            dn_request     : IN STD_LOGIC_VECTOR (N downto 2);       -- down buttons on each floor
            
            up_status      : IN STD_LOGIC;
            down_status    : IN STD_LOGIC;
            open_status    : IN STD_LOGIC;
            floor_status   : IN STD_LOGIC_VECTOR (N downto 1);       -- same size as "b" input

            req            : OUT STD_LOGIC_VECTOR (N downto 1)       -- same size as "floor_status" and "b" input
            );
END Request_Resolver;

ARCHITECTURE Behavioral OF Request_Resolver IS
    SIGNAL reg_b              : STD_LOGIC_VECTOR (N downto 1);       -- registered floor button requests
    SIGNAL reg_up_request     : STD_LOGIC_VECTOR (N-1 downto 1);     -- registered up requests
    SIGNAL reg_dn_request     : STD_LOGIC_VECTOR (N downto 2);       -- registered down requests
    SIGNAL reg_up_status,
           reg_down_status,
           assumed_up_status    : STD_LOGIC;
    SIGNAL requests, temp_same_floor_request_flag, up_requests, down_requests, down_requests_reversed, reversed_down_floor, down_floor           : STD_LOGIC_VECTOR (N downto 1);       -- combining the registered requests from the 
                                                                     -- buttons both inside and outside the elevator.
    SIGNAL temp_floor_status: STD_LOGIC_VECTOR (N +1 downto 1);
    SIGNAL  up_requests_flag,
            down_requests_flag,
            same_floor_request_flag : STD_LOGIC;
    SIGNAL  up_flag,
            down_flag,
            stay_idle_flag,
            open_the_door_flag: STD_LOGIC;
            
BEGIN
    -- registering the input signals (b, up_request, dn_request) and Feedback signals (up_status, down_status).
    process (clk, reset_n)
    begin
        IF (reset_n = '0') THEN
            reg_b           <= (others => '0');

            reg_up_request  <= (others => '0');
            reg_dn_request  <= (others => '0');

            reg_up_status   <= '0';
            reg_down_status <= '0';
        ELSIF rising_edge(clk) THEN
            -- registering 'b' whenever it is pressed, and deasserting the registered signal when the 
            -- requested floor is reached and the door is open.
            FOR i in 1 to N LOOP
                IF      (floor_status(i) = '1' AND open_status = '1') THEN 
                    reg_b(i) <= '0';
                ELSIF   (reg_b(i) = '0' AND b(i) = '1') THEN
                    reg_b(i) <= b(i);
                END IF;
            END LOOP;

            FOR i in 1 to N-1  LOOP
                -- registering 'up_request' whenever it is pressed, and deasserting the registered signal when the 
                -- requested floor is reached and the door is open.
                IF      (floor_status(i) = '1' AND open_status = '1') THEN reg_up_request(i) <= '0';
                ELSIF   (reg_up_request(i) = '0' AND up_request(i) = '1') THEN
                    reg_up_request(i) <= up_request(i);
                END IF;
                -- registering 'dn_request' whenever it is pressed, and deasserting the registered signal when the 
                -- requested floor is reached and the door is open.
                IF      (floor_status(i+1) = '1' AND open_status = '1') THEN reg_dn_request(i+1) <= '0';
                ELSIF   (reg_dn_request(i+1) = '0' AND dn_request(i+1) = '1') THEN
                    reg_dn_request(i+1) <= dn_request(i+1);
                END IF;
            END LOOP;
            
            -- registering the Feedback signals (up_status, down_status)
            IF (open_status = '0') THEN
                reg_up_status <= up_status;
                reg_down_status <= down_status;
            END IF;
        END IF;
    end process;
    
    requests <= reg_b OR ('0' & reg_up_request) OR (reg_dn_request & '0');
    -- filtering the up requests and asserting flag if there are up requests
    temp_floor_status <= NOT (std_logic_vector(unsigned(floor_status & '0') - 1));
    up_requests <= requests AND temp_floor_status (N downto 1);                                                                          -- test corners
    up_requests_flag <= '1' when up_requests > (N downto 1 => '0')  else
                        '0';

    -- filtering the down requests and asserting flag if there are down requests
    down_requests <= requests AND std_logic_vector(unsigned(floor_status) - 1);
    down_requests_flag <=   '1' when down_requests > (N downto 1 => '0')  else
                            '0';

    -- asserting flag if there is a request at the same floor of the elevator during the IDLE state
    temp_same_floor_request_flag <= requests AND floor_status;
    same_floor_request_flag <=  '1' when temp_same_floor_request_flag > (N downto 1 => '0')  else
                                '0';
    
    -- assuming a direction if the elevator in the ideal state
    assumed_up_status <= '1' when floor_status >= STD_LOGIC_VECTOR(to_unsigned(natural(ceil(real(N/2))), N)) else                                                           -- modify
                         '0';
                         
    up_flag <=  (reg_up_status AND up_requests_flag) OR 
                (reg_down_status AND NOT (down_requests_flag) AND up_requests_flag) OR 
                (NOT(reg_up_status) AND NOT(reg_down_status) AND assumed_up_status AND up_requests_flag) OR
                (NOT(reg_up_status) AND NOT(reg_down_status) AND NOT(assumed_up_status) AND up_requests_flag AND NOT(down_requests_flag));
    
    down_flag <=  (reg_down_status AND down_requests_flag) OR 
                  (reg_up_status AND NOT (up_requests_flag) AND down_requests_flag) OR 
                  (NOT(reg_up_status) AND NOT(reg_down_status) AND NOT(assumed_up_status) AND down_requests_flag) OR
                  (NOT(reg_up_status) AND NOT(reg_down_status) AND assumed_up_status AND NOT(up_requests_flag) AND down_requests_flag);
    
    open_the_door_flag <= NOT(reg_up_status) AND NOT(reg_down_status) AND same_floor_request_flag;

    stay_idle_flag <= NOT(up_requests_flag) AND NOT(down_requests_flag) AND NOT(same_floor_request_flag);

    -- calculating the targeted floor in the downward direction
    reverse_gen: FOR i IN 1 TO N GENERATE
        down_requests_reversed(i) <= down_requests(N + 1 - i);
    END GENERATE;

    reversed_down_floor <= (down_requests_reversed AND std_logic_vector(unsigned (not down_requests_reversed) + 1));

    reverse_back_gen: FOR i IN 1 TO N GENERATE
        down_floor(i) <= reversed_down_floor(N + 1 - i);
    END GENERATE;

    req <=  floor_status                                                        when open_the_door_flag = '1' else
            (up_requests AND std_logic_vector(unsigned (not up_requests) + 1))  when up_flag = '1'            else
            down_floor                                                          when down_flag = '1'          else
            (others => '0');

END Behavioral;

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

---------------------------------------------------------------------------------------------------------
--                                               counter                                               --
---------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;
USE ieee.numeric_std.ALL;

ENTITY counter IS
    GENERIC ( n_bits : NATURAL := 4 ; k: NATURAL := 16);
    PORT (  clock   : IN STD_LOGIC;
            reset_n : IN STD_LOGIC;     -- synchronous reset        
            En      : IN STD_LOGIC;
            Load    : IN STD_LOGIC;
            data_in : IN STD_LOGIC_VECTOR(n_bits-1 downto 0);

            rollover: OUT STD_LOGIC );
END counter;

ARCHITECTURE Behavior OF counter IS
    SIGNAL value : STD_LOGIC_VECTOR (n_bits-1 DOWNTO 0);
BEGIN
    PROCESS (clock)
    BEGIN
        IF rising_edge (clock) THEN
            IF (reset_n = '0' AND Load = '1') THEN
                value <= data_in;
            ELSIF (reset_n = '0') THEN
                value <= (OTHERS => '0');
            ELSIF ((value < k-1) AND (En = '1')) THEN
                value <= std_logic_vector(unsigned(value) + 1);
            ELSIF (En = '1') THEN
                value <= (others =>'0');
            END IF;
        END IF;
    END PROCESS;
    rollover <= '1' when value = k-1 
                    else '0';
END Behavior;

