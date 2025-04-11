-- LEDController.vhd
-- Updated date: 2025.04.11
-- SCOMP Peripheral: LED Controller with
-- - Individual LED on/off
-- - Per-LED brightness control
-- - Per-LED gamma correction toggle
-- - Global pulse mode (ramping brightness)
-- - Per-LED timed pulsing
-- - Ripple mode (gradual lighting from LED 0 to LED 9)

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY LEDController IS
PORT(
    CS          : IN  STD_LOGIC;
    WRITE_EN    : IN  STD_LOGIC;
    RESETN      : IN  STD_LOGIC;
    LEDs        : OUT STD_LOGIC_VECTOR(9 DOWNTO 0);
    IO_DATA     : IN  STD_LOGIC_VECTOR(15 DOWNTO 0)
);
END LEDController;

ARCHITECTURE Behavioral OF LEDController IS

    SIGNAL led_state       : STD_LOGIC_VECTOR(9 DOWNTO 0) := (OTHERS => '0');

    TYPE brightness_array IS ARRAY(0 TO 9) OF STD_LOGIC_VECTOR(3 DOWNTO 0);
    TYPE gamma_array IS ARRAY(0 TO 9) OF STD_LOGIC;

    SIGNAL led_brightness  : brightness_array := (OTHERS => "1111");
    SIGNAL led_gamma       : gamma_array := (OTHERS => '1');
    SIGNAL pwm_counter     : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";
    SIGNAL pwm_clk         : STD_LOGIC := '0';
    SIGNAL clk_div         : INTEGER RANGE 0 TO 50000 := 0;

    TYPE gamma_table_type IS ARRAY(0 TO 15) OF STD_LOGIC_VECTOR(3 DOWNTO 0);
    CONSTANT gamma_table : gamma_table_type := (
        "0000", "0000", "0001", "0001", "0010", "0011", "0100", "0101",
        "0111", "1000", "1001", "1010", "1100", "1101", "1110", "1111"
    );
    -- ripple mode variables
    TYPE pulse_array IS ARRAY(0 TO 9) OF STD_LOGIC;
    TYPE pulse_val_array IS ARRAY(0 TO 9) OF STD_LOGIC_VECTOR(3 DOWNTO 0);
    TYPE pulse_count_array IS ARRAY(0 TO 9) OF INTEGER RANGE 0 TO 600000;

    SIGNAL led_pulse_enabled : pulse_array := (OTHERS => '0');
    SIGNAL led_pulse_target  : pulse_val_array := (OTHERS => "0000");
    SIGNAL led_pulse_counter : pulse_count_array := (OTHERS => 0);
    SIGNAL led_pulse_mode    : pulse_array := (OTHERS => '0');
    SIGNAL led_pulse_flag    : pulse_array := (OTHERS => '0');
    SIGNAL pulse_clock       : INTEGER RANGE 0 TO 100000 := 0;

    --ripple mode variables
    SIGNAL ripple_mode       : STD_LOGIC := '0';
    SIGNAL ripple_index      : INTEGER RANGE 0 TO 9 := 0;
    SIGNAL ripple_counter    : INTEGER RANGE 0 TO 100000 := 0;
BEGIN

    PROCESS (CS)
    BEGIN
        IF rising_edge(CS) THEN
            IF clk_div = 50 THEN
                pwm_clk <= NOT pwm_clk;
                clk_div <= 0;
            ELSE
                clk_div <= clk_div + 1;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (pwm_clk)
    BEGIN
        IF rising_edge(pwm_clk) THEN
            IF pwm_counter = "1111" THEN
                pwm_counter <= "0000";
            ELSE
                pwm_counter <= pwm_counter + 1;
            END IF;
        END IF;
    END PROCESS;

-- SCOMP Interface: Handle Commands + Pulse Logic
PROCESS (RESETN, CS)
    VARIABLE command : STD_LOGIC_VECTOR(3 DOWNTO 0);
    VARIABLE led_index : INTEGER RANGE 0 TO 9;
BEGIN
    IF RESETN = '0' THEN
        led_state <= (OTHERS => '0');
        led_brightness <= (OTHERS => "1111");
        led_gamma <= (OTHERS => '1');
        -- pulse mode variables
        led_pulse_mode <= (OTHERS => '0');
        pulse_clock <= 0;
        led_pulse_flag <= (OTHERS => '0');
        led_pulse_enabled <= (OTHERS => '0');
        led_pulse_target <= (OTHERS => "0000");
        led_pulse_counter <= (OTHERS => 0);
		  
	    --ripple mode variables
        ripple_mode <= '0';
        ripple_index <= 0;
        ripple_counter <= 0;

    ELSIF rising_edge(CS) THEN

        -- Handle WRITE commands
        IF WRITE_EN = '1' THEN
            command := IO_DATA(15 DOWNTO 12);
            led_index := CONV_INTEGER(IO_DATA(11 DOWNTO 8));

            CASE command IS
                WHEN "0000" =>  -- F0 Set all LEDs directly
                    led_state <= IO_DATA(9 DOWNTO 0);

                WHEN "0001" =>  -- F1 Set individual LED ON
                    IF led_index <= 9 THEN
                        led_state(led_index) <= '1';
                    END IF;

                WHEN "0010" =>  -- F2 Clear individual LED
                    IF led_index <= 9 THEN
                        led_state(led_index) <= '0';
                    END IF;

                WHEN "0011" =>  -- F3 Toggle individual LED
                    IF led_index <= 9 THEN
                        led_state(led_index) <= NOT led_state(led_index);
                    END IF;

                WHEN "0100" =>  -- F4 Set individual brightness + gamma
                    IF led_index <= 9 THEN
                        led_brightness(led_index) <= IO_DATA(3 DOWNTO 0);
                        led_gamma(led_index) <= IO_DATA(4);
                        led_state(led_index) <= '1';
                    END IF;

                WHEN "0101" =>  -- F5 Set all brightness + gamma
                    FOR i IN 0 TO 9 LOOP
                        led_brightness(i) <= IO_DATA(3 DOWNTO 0);
                        led_gamma(i) <= IO_DATA(4);
                        led_state(i) <= '1';
                    END LOOP;
						  
				    WHEN "0110" => -- F6 all the ligh pulsing
						  FOR i IN 0 TO 9 LOOP
                        led_pulse_enabled(i) <= '1';
                        led_pulse_target(i) <= IO_DATA(3 DOWNTO 0);
                        led_pulse_counter(i) <= 0;
                    END LOOP;

                WHEN "0111" =>  -- F7: Individual pulse mode
                    IF led_index <= 9 THEN
                        led_pulse_enabled(led_index) <= '1';
                        led_pulse_target(led_index) <= IO_DATA(3 DOWNTO 0);
                        led_pulse_counter(led_index) <= 0;
                    END IF;
						  


                WHEN "1010" =>  -- F8: ripple mode
                    ripple_mode <= '1';
                    ripple_index <= 0;


                WHEN OTHERS =>
                    NULL;
            END CASE;
        END IF;

        -- Ripple mode implementation
        IF ripple_mode = '1' THEN
            -- Increment counter for timing
            IF ripple_counter < 50000 THEN
                ripple_counter <= ripple_counter + 1;
            ELSE
                ripple_counter <= 0;
                
                -- Set current LED to full brightness
                led_state(ripple_index) <= '1';
                led_brightness(ripple_index) <= "1111";
                led_gamma(ripple_index) <= '1';
                
                -- Move to next LED
                IF ripple_index < 9 THEN
                    ripple_index <= ripple_index + 1;
                ELSE
                    -- Reset back to LED 0 when we reach the end
                    ripple_index <= 0;
                    -- Optionally reset all LEDs to create wave effect
                    led_state <= (OTHERS => '0');
                END IF;
            END IF;
        END IF;

        -- Update all LEDs with current brightness
        FOR i IN 0 TO 9 LOOP
            IF led_pulse_enabled(i) = '1' THEN
				-- once enabled this means the light would start to pulse
                IF led_pulse_counter(i) < (37500 * (CONV_INTEGER(led_pulse_target(i)) + 1)) THEN
                    led_pulse_counter(i) <= led_pulse_counter(i) + 1;
                ELSE
                    led_pulse_counter(i) <= 0;
                    IF led_pulse_flag(i) = '0' THEN
                        -- We're in the increasing phase
                        IF led_brightness(i) = "1111" THEN
                            led_pulse_flag(i) <= '1';
                            led_brightness(i) <= led_brightness(i) - 1;
                        ELSE
                            led_brightness(i) <= led_brightness(i) + 1;
                        END IF;
                    ELSE
                        -- We're in the decreasing phase
                        IF led_brightness(i) = "0000" THEN
                            led_pulse_flag(i) <= '0';
                            led_brightness(i) <= led_brightness(i) + 1;
                        ELSE
                            led_brightness(i) <= led_brightness(i) - 1;
                        END IF;
                    END IF;
                    led_gamma(i) <= '1';
                    led_state(i) <= '1';
                END IF;
            END IF;
        END LOOP;

    END IF;
END PROCESS;

    PROCESS (led_state, pwm_counter, led_brightness, led_gamma, led_pulse_enabled)
        VARIABLE temp_output : STD_LOGIC_VECTOR(9 DOWNTO 0);
        VARIABLE effective_brightness : STD_LOGIC_VECTOR(3 DOWNTO 0);
    BEGIN
        FOR i IN 0 TO 9 LOOP
            IF led_gamma(i) = '1' THEN
                effective_brightness := gamma_table(CONV_INTEGER(led_brightness(i)));
            ELSE
                effective_brightness := led_brightness(i);
            END IF;

            IF led_state(i) = '1' AND pwm_counter < effective_brightness THEN
                temp_output(i) := '1';
            ELSE
                temp_output(i) := '0';
            END IF;
        END LOOP;

        LEDs <= temp_output;
    END PROCESS;

END Behavioral;
