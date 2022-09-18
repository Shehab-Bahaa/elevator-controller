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
    up_requests <= requests AND temp_floor_status (N downto 1);
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
    assumed_up_status <= '1' when floor_status >= STD_LOGIC_VECTOR(to_unsigned(natural(ceil(real(N/2))), N)) else 
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
