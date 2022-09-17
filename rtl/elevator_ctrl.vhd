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
