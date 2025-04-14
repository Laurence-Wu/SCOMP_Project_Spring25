-- LEDController.vhd
-- Updated date: 2025.04.12 (Refined with Ripple Effect)
-- SCOMP Peripheral: LED Controller
--
-- Features:
--   - Individual and multiple LED brightness control
--   - Positive/negative pulse modes, both global and per-LED
--   - Gamma correction applied via a lookup table
--   - Ripple mode: gradual lighting from LED 0 to LED 9
--
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

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

    ----------------------------------------------------------------------------
    -- LED State and Brightness Control Signals
    ----------------------------------------------------------------------------
    SIGNAL led_state       : STD_LOGIC_VECTOR(9 DOWNTO 0) := (OTHERS => '0');
    
    TYPE brightness_array IS ARRAY(0 TO 9) OF STD_LOGIC_VECTOR(3 DOWNTO 0);
    TYPE gamma_array      IS ARRAY(0 TO 9) OF STD_LOGIC;
    SIGNAL led_brightness  : brightness_array := (OTHERS => "1111");
    SIGNAL led_gamma       : gamma_array      := (OTHERS => '1');

    ----------------------------------------------------------------------------
    -- PWM Generation Signals
    ----------------------------------------------------------------------------
    SIGNAL pwm_counter     : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";
    SIGNAL pwm_clk         : STD_LOGIC := '0';
    SIGNAL clk_div         : INTEGER RANGE 0 TO 50000 := 0;

    TYPE gamma_table_type IS ARRAY(0 TO 15) OF STD_LOGIC_VECTOR(3 DOWNTO 0);
    CONSTANT gamma_table : gamma_table_type := (
        "0000", "0000", "0001", "0001", "0010", "0011", "0100", "0101",
        "0111", "1000", "1001", "1010", "1100", "1101", "1110", "1111"
    );

    ----------------------------------------------------------------------------
    -- Global Positive Pulsing Signals
    ----------------------------------------------------------------------------
    SIGNAL pulse_mode        : STD_LOGIC := '0';
    SIGNAL pulse_brightness  : STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";
    SIGNAL k                 : INTEGER RANGE 0 TO 74999 := 0;

    ----------------------------------------------------------------------------
    -- Global Negative Pulsing Signals
    ----------------------------------------------------------------------------
    SIGNAL neg_pulse_mode        : STD_LOGIC := '0';
    SIGNAL neg_pulse_brightness  : STD_LOGIC_VECTOR(3 DOWNTO 0) := "1111";
    SIGNAL neg_k                 : INTEGER RANGE 0 TO 74999 := 0;

    ----------------------------------------------------------------------------
    -- Per-LED Pulse Controls
    ----------------------------------------------------------------------------
    TYPE pulse_array       IS ARRAY(0 TO 9) OF STD_LOGIC;
    TYPE pulse_val_array   IS ARRAY(0 TO 9) OF STD_LOGIC_VECTOR(3 DOWNTO 0);
    TYPE pulse_count_array IS ARRAY(0 TO 9) OF INTEGER RANGE 0 TO 600000;

    SIGNAL led_pulse_enabled : pulse_array       := (OTHERS => '0');
    SIGNAL led_pulse_target  : pulse_val_array   := (OTHERS => "0000");
    SIGNAL led_pulse_counter : pulse_count_array := (OTHERS => 0);

    SIGNAL led_neg_enabled   : pulse_array       := (OTHERS => '0');
    SIGNAL led_neg_target    : pulse_val_array   := (OTHERS => "0000");
    SIGNAL led_neg_counter   : pulse_count_array := (OTHERS => 0);

    ----------------------------------------------------------------------------
    -- Ripple Mode Signals
    ----------------------------------------------------------------------------
    SIGNAL ripple_mode    : STD_LOGIC := '0';
    SIGNAL ripple_index   : INTEGER RANGE 0 TO 9 := 0;
    SIGNAL ripple_counter : INTEGER RANGE 0 TO 100000 := 0;

BEGIN

    ----------------------------------------------------------------------------
    -- Clock Generation Process
    -- Generates pwm_clk based on CS with a simple clock divider.
    ----------------------------------------------------------------------------
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

    ----------------------------------------------------------------------------
    -- PWM Counter Process
    -- Increments pwm_counter on each rising edge of pwm_clk.
    ----------------------------------------------------------------------------
    PROCESS (pwm_clk)
    BEGIN
        IF rising_edge(pwm_clk) THEN
            IF pwm_counter = "1111" THEN
                pwm_counter <= (OTHERS => '0');
            ELSE
                pwm_counter <= std_logic_vector(unsigned(pwm_counter) + 1);
            END IF;
        END IF;
    END PROCESS;

    ----------------------------------------------------------------------------
    -- Command Processing, Pulse, and Ripple Control Process
    -- Processes commands from IO_DATA and updates LED control signals.
    ----------------------------------------------------------------------------
    PROCESS (RESETN, CS)
        VARIABLE command         : STD_LOGIC_VECTOR(1 DOWNTO 0);
        VARIABLE led_index       : INTEGER RANGE 0 TO 9;
        VARIABLE led_index_array : STD_LOGIC_VECTOR(9 DOWNTO 0);
    BEGIN
        IF RESETN = '0' THEN
            -- Reset all LED states, brightness, gamma correction, and pulse settings.
            led_state         <= (OTHERS => '0');
            led_brightness    <= (OTHERS => "1111");
            led_gamma         <= (OTHERS => '1');
            pulse_mode        <= '0';
            pulse_brightness  <= "0000";
            k                 <= 0;
            neg_pulse_mode    <= '0';
            neg_pulse_brightness <= "1111";
            neg_k             <= 0;
            led_pulse_enabled <= (OTHERS => '0');
            led_pulse_target  <= (OTHERS => "0000");
            led_pulse_counter <= (OTHERS => 0);
            led_neg_enabled   <= (OTHERS => '0');
            led_neg_target    <= (OTHERS => "0000");
            led_neg_counter   <= (OTHERS => 0);
            ripple_mode       <= '0';
            ripple_index      <= 0;
            ripple_counter    <= 0;
            
        ELSIF rising_edge(CS) THEN
            IF WRITE_EN = '1' THEN
                command := IO_DATA(15 DOWNTO 14);
                CASE command IS
                    ----------------------------------------------------------------------------
                    -- Command "00": Set Brightness for Multiple LEDs
                    ----------------------------------------------------------------------------
                    WHEN "00" =>
                        led_index_array := IO_DATA(9 DOWNTO 0);
                        FOR i IN 0 TO 9 LOOP
                            IF led_index_array(i) = '1' THEN
                                led_brightness(i) <= IO_DATA(13 DOWNTO 10);
                                led_gamma(i)      <= '1';
                                led_state(i)      <= '1';
                            END IF;
                        END LOOP;
                    ----------------------------------------------------------------------------
                    -- Command "01": Set Positive Pulse for Multiple LEDs
                    ----------------------------------------------------------------------------
                    WHEN "01" =>
                        led_index_array := IO_DATA(9 DOWNTO 0);
                        FOR i IN 0 TO 9 LOOP
                            IF led_index_array(i) = '1' THEN
                                led_pulse_enabled(i) <= '1';
                                led_pulse_target(i)  <= IO_DATA(13 DOWNTO 10);
                                led_pulse_counter(i) <= 0;
                            END IF;
                        END LOOP;
                    ----------------------------------------------------------------------------
                    -- Command "10": Set Negative Pulse for Multiple LEDs
                    ----------------------------------------------------------------------------
                    WHEN "10" =>
                        led_index_array := IO_DATA(9 DOWNTO 0);
                        FOR i IN 0 TO 9 LOOP
                            IF led_index_array(i) = '1' THEN
                                led_neg_enabled(i) <= '1';
                                led_neg_target(i)  <= IO_DATA(13 DOWNTO 10);
                                led_neg_counter(i) <= 0;
                                led_state(i)       <= '1';
                                led_gamma(i)       <= '1';
                            END IF;
                        END LOOP;
                    ----------------------------------------------------------------------------
                    -- Command "11": Single-LED Functions (using IO_DATA(13 DOWNTO 10) for further selection)
                    ----------------------------------------------------------------------------
                    WHEN "11" =>
                        IF IO_DATA(13 DOWNTO 10) = "0000" THEN  -- F3: Toggle Multiple LEDs(Not fully implemented)
                            led_index_array := IO_DATA(9 DOWNTO 0);
                            FOR i IN 0 TO 9 LOOP
                                IF led_index_array(i) = '1' THEN
                                    led_state(i) <= NOT led_state(i);
                                    led_pulse_counter(i) <= 0;
                                END IF;
                            END LOOP;
                        ELSIF IO_DATA(13 DOWNTO 10) = "0001" THEN  -- F4: Toggle Single LED(Not fully implemented)
                            led_index := to_integer(unsigned(IO_DATA(3 DOWNTO 0)));
                            IF led_index <= 9 THEN
                                led_state(led_index) <= NOT led_state(led_index);
                            END IF;
                        ELSIF IO_DATA(13 DOWNTO 10) = "0010" THEN  -- F5: Set Brightness for Single LED
                            led_index := to_integer(unsigned(IO_DATA(3 DOWNTO 0)));
                            IF led_index <= 9 THEN
                                led_brightness(led_index) <= IO_DATA(7 DOWNTO 4);
                                led_gamma(led_index)      <= '1';  -- Enable gamma correction.
                                led_state(led_index)      <= '1';
                            END IF;
                        ELSIF IO_DATA(13 DOWNTO 10) = "0011" THEN  -- F6: Set Positive Pulse for Single LED
                            led_index := to_integer(unsigned(IO_DATA(3 DOWNTO 0)));
                            IF led_index <= 9 THEN
                                led_pulse_enabled(led_index) <= '1';
                                led_pulse_target(led_index)  <= IO_DATA(7 DOWNTO 4);
                                led_pulse_counter(led_index) <= 0;
                            END IF;
                        ELSIF IO_DATA(13 DOWNTO 10) = "0100" THEN  -- F7: Set Negative Pulse for Single LED
                            led_index := to_integer(unsigned(IO_DATA(3 DOWNTO 0)));
                            IF led_index <= 9 THEN
                                led_neg_enabled(led_index) <= '1';
                                led_neg_target(led_index)  <= IO_DATA(7 DOWNTO 4);
                                led_neg_counter(led_index) <= 0;
                                led_state(led_index)       <= '1';
                                led_gamma(led_index)       <= '1';
                            END IF;
                        ELSIF IO_DATA(13 DOWNTO 10) = "0101" THEN  -- F8: Blinking Effect (Not Implemented)
                            -- TODO: Implement blinking effect logic.
                        ELSIF IO_DATA(13 DOWNTO 10) = "0110" THEN  -- F9: Global Positive Pulsing Effect
                            pulse_mode <= '1';
                            k <= 0;
                        ELSIF IO_DATA(13 DOWNTO 10) = "0111" THEN  -- F10: Ripple Effect
                            led_index := to_integer(unsigned(IO_DATA(3 DOWNTO 0)));
                            IF led_index <= 9 THEN
                                ripple_mode  <= '1';
                                ripple_index <= led_index;  -- Use specified index as starting point
                                ripple_counter <= 0;
                                -- Optionally, set the starting LED
                                led_state(ripple_index) <= '1';
                            END IF;
								ELSIF IO_DATA(13 DOWNTO 10) = "1000" THEN  -- F11: Reset all variables
									led_state         <= (OTHERS => '0');
									led_brightness    <= (OTHERS => "1111");
									led_gamma         <= (OTHERS => '1');
									pulse_mode        <= '0';
									pulse_brightness  <= "0000";
									k                 <= 0;
									neg_pulse_mode    <= '0';
									neg_pulse_brightness <= "1111";
									neg_k             <= 0;
									led_pulse_enabled <= (OTHERS => '0');
									led_pulse_target  <= (OTHERS => "0000");
									led_pulse_counter <= (OTHERS => 0);
									led_neg_enabled   <= (OTHERS => '0');
									led_neg_target    <= (OTHERS => "0000");
									led_neg_counter   <= (OTHERS => 0);
									ripple_mode       <= '0';
									ripple_index      <= 0;
									ripple_counter    <= 0;
								END IF;
                    ----------------------------------------------------------------------------
                    WHEN OTHERS =>
                        NULL;
                END CASE;
            END IF;  -- End WRITE_EN handling

            ----------------------------------------------------------------------------
            -- Ripple Mode Processing
            ----------------------------------------------------------------------------
            IF ripple_mode = '1' THEN
                IF ripple_counter < 50000 THEN
                    ripple_counter <= ripple_counter + 1;
                ELSE
                    ripple_counter <= 0;
                    -- Set current LED to full brightness with gamma enabled
                    led_state(ripple_index) <= '1';
                    led_brightness(ripple_index) <= "1111";
                    led_gamma(ripple_index) <= '1';
                    -- Move to next LED in sequence
                    IF ripple_index < 9 THEN
                        ripple_index <= ripple_index + 1;
                    ELSE
                        ripple_index <= 0;
                        -- Optionally, reset all LEDs to create a wave effect:
                        led_state <= (OTHERS => '0');
                        -- Optionally, stop ripple mode after one full cycle:
                        ripple_mode <= '0';
                    END IF;
                END IF;
            END IF;

            ----------------------------------------------------------------------------
            -- Global Positive Pulsing Effect Processing
            ----------------------------------------------------------------------------
            IF pulse_mode = '1' THEN
                IF k < 74999 THEN
                    k <= k + 1;
                ELSE
                    k <= 0;
                    IF pulse_brightness < "1111" THEN
                        pulse_brightness <= std_logic_vector(unsigned(pulse_brightness) + 1);
                    ELSE
                        pulse_brightness <= "0000";
                    END IF;
                    FOR i IN 0 TO 9 LOOP
                        led_brightness(i) <= pulse_brightness;
                        led_gamma(i)      <= '1';
                        led_state(i)      <= '1';
                    END LOOP;
                END IF;
            END IF;

            ----------------------------------------------------------------------------
            -- Individual Positive Pulsing Processing for LEDs
            ----------------------------------------------------------------------------
            FOR i IN 0 TO 9 LOOP
                IF led_pulse_enabled(i) = '1' THEN
                    IF led_pulse_counter(i) < (37500 * (to_integer(unsigned(led_pulse_target(i))) + 1)) THEN
                        led_pulse_counter(i) <= led_pulse_counter(i) + 1;
                    ELSE
                        led_pulse_counter(i) <= 0;
                        IF led_brightness(i) < "1111" THEN
                            led_brightness(i) <= std_logic_vector(unsigned(led_brightness(i)) + 1);
                        ELSE
                            led_brightness(i) <= "0000";
                        END IF;
                        led_gamma(i) <= '1';
                        led_state(i) <= '1';
                    END IF;
                END IF;
            END LOOP;

            ----------------------------------------------------------------------------
            -- Global Negative Pulsing Processing
            ----------------------------------------------------------------------------
            IF neg_pulse_mode = '1' THEN
                IF neg_k < 74999 THEN
                    neg_k <= neg_k + 1;
                ELSE
                    neg_k <= 0;
                    IF neg_pulse_brightness > "0000" THEN
                        neg_pulse_brightness <= std_logic_vector(unsigned(neg_pulse_brightness) - 1);
                    ELSE
                        neg_pulse_brightness <= "1111";
                    END IF;
                    FOR i IN 0 TO 9 LOOP
                        led_brightness(i) <= neg_pulse_brightness;
                        led_gamma(i)      <= '1';
                        led_state(i)      <= '1';
                    END LOOP;
                END IF;
            END IF;

            ----------------------------------------------------------------------------
            -- Individual Negative Pulsing Processing for LEDs
            ----------------------------------------------------------------------------
            FOR i IN 0 TO 9 LOOP
                IF led_neg_enabled(i) = '1' THEN
                    IF led_neg_counter(i) < (37500 * (to_integer(unsigned(led_neg_target(i))) + 1)) THEN
                        led_neg_counter(i) <= led_neg_counter(i) + 1;
                    ELSE
                        led_neg_counter(i) <= 0;
                        IF led_brightness(i) > "0000" THEN
                            led_brightness(i) <= std_logic_vector(unsigned(led_brightness(i)) - 1);
                        ELSE
                            led_brightness(i) <= "0000";
                            led_state(i)      <= '0';
                        END IF;
                        led_gamma(i) <= '1';
                    END IF;
                END IF;
            END LOOP;

        END IF;
    END PROCESS;

    ----------------------------------------------------------------------------
    -- PWM Output Process
    -- Calculates effective brightness (with gamma correction if enabled) and
    -- generates the LED output based on pwm_counter.
    ----------------------------------------------------------------------------
    PROCESS (led_state, pwm_counter, led_brightness, led_gamma)
        VARIABLE temp_output          : STD_LOGIC_VECTOR(9 DOWNTO 0);
        VARIABLE effective_brightness : STD_LOGIC_VECTOR(3 DOWNTO 0);
    BEGIN
        FOR i IN 0 TO 9 LOOP
            IF led_gamma(i) = '1' THEN
                effective_brightness := gamma_table(to_integer(unsigned(led_brightness(i))));
            ELSE
                effective_brightness := led_brightness(i);
            END IF;
            IF led_state(i) = '1' AND unsigned(pwm_counter) < unsigned(effective_brightness) THEN
                temp_output(i) := '1';
            ELSE
                temp_output(i) := '0';
            END IF;
        END LOOP;
        LEDs <= temp_output;
    END PROCESS;

END Behavioral;
