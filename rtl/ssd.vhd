LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY ssd IS   -- seven-segment display decoder
    PORT (
        one_hot_number  : IN std_logic_vector(15 downto 1);
        seg             : OUT std_logic_vector(6 downto 0)
    );
END ssd ;

ARCHITECTURE Behavioral OF ssd IS           
    
BEGIN
    seg <=      "1000000" when one_hot_number = "000000000000001"  -- 0
           else "1111001" when one_hot_number = "000000000000010"  -- 1
           else "0100100" when one_hot_number = "000000000000100"  -- 2
           else "0110000" when one_hot_number = "000000000001000"  -- 3
           else "0011001" when one_hot_number = "000000000010000"  -- 4
           else "0010010" when one_hot_number = "000000000100000"  -- 5
           else "0000010" when one_hot_number = "000000001000000"  -- 6
           else "1111000" when one_hot_number = "000000010000000"  -- 7
           else "0000000" when one_hot_number = "000000100000000"  -- 8
           else "0010000" when one_hot_number = "000001000000000"  -- 9
           else "0001000" when one_hot_number = "000010000000000"  -- A (10)
           else "0000011" when one_hot_number = "000100000000000"  -- B (11)
           else "1000110" when one_hot_number = "001000000000000"  -- C (12)
           else "0100001" when one_hot_number = "010000000000000"  -- D (13)
           else "0000110" when one_hot_number = "100000000000000"  -- E (14)
           else (Others => '1');
END Behavioral;