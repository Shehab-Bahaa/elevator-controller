---------------------------------------------------------------------------------------------------------
--                                        Subprograms Package                                          --
---------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.all;

PACKAGE tb_subprograms IS 
    Function OneHot_to_Decimal (OneHot: STD_LOGIC_VECTOR) RETURN INTEGER;
    Function Decimal_to_OneHot (Decimal: INTEGER; Size: NATURAL) RETURN STD_LOGIC_VECTOR;
END;

PACKAGE BODY tb_subprograms IS

    Function OneHot_to_Decimal (OneHot: STD_LOGIC_VECTOR) 
        RETURN INTEGER
    IS 
        variable OneHot_var : std_logic_vector (OneHot'length-1 downto 0) := OneHot;
        variable Decimal: INTEGER := 0;
    BEGIN
        FOR i in OneHot_var'RANGE LOOP
            IF (OneHot_var(i) = '1') THEN
                Decimal := i;
                RETURN Decimal;
            END IF; 
        END LOOP;
    END;

    Function Decimal_to_OneHot (Decimal: INTEGER; Size: NATURAL) 
        RETURN STD_LOGIC_VECTOR
    IS 
        variable OneHot : std_logic_vector (Size-1 downto 0) := (others => '0');
    BEGIN
        OneHot (Decimal) := '1';
        RETURN OneHot;
    END;

END;

