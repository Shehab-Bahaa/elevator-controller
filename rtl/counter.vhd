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
