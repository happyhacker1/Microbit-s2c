object s2c {
    def main(args: Array[String]) = {

        import java.io.PrintWriter

        print("Insert your message here: ")
        val input: String = scala.io.StdIn.readLine()

        var flowString = new Array[String](5) // 5 long strings, each representing a row

        def capitalize(c: Char) : Char = {
            c match {
                case 'a' => 'A'
                case 'b' => 'B'
                case 'c' => 'C'
                case 'd' => 'D'
                case 'e' => 'E'
                case 'f' => 'F'
                case 'g' => 'G'
                case 'h' => 'H'
                case 'i' => 'I'
                case 'j' => 'J'
                case 'k' => 'K'       
                case 'l' => 'L'
                case 'm' => 'M'
                case 'n' => 'N'
                case 'o' => 'O'
                case 'p' => 'P'
                case 'q' => 'Q'
                case 'r' => 'R'
                case 's' => 'S'
                case 't' => 'T'
                case 'u' => 'U'
                case 'v' => 'V'
                case 'w' => 'W'
                case 'x' => 'X'
                case 'y' => 'Y'
                case 'z' => 'Z'
                case x: Char => x
            }
        }

        def mkArrayString(c: Char) : Array[String] = {
            c match {
                case 'A' => Array("oxxoo","xooxo","xxxxo","xooxo","xooxo")
                case 'B' => Array("xxxoo","xooxo","xxxoo","xooxo","xxxoo")
                case 'C' => Array("oxxoo","xooxo","xoooo","xooxo","oxxoo")
                case 'D' => Array("xxxoo","xooxo","xooxo","xooxo","xxxoo")
                case 'E' => Array("xxxxo","xoooo","xxxoo","xoooo","xxxxo")
                case 'F' => Array("xxxxo","xoooo","xxxoo","xoooo","xoooo")
                case 'G' => Array("oxxoo","xoooo","xoxxo","xooxo","oxxoo")
                case 'H' => Array("xooxo","xooxo","xxxxo","xooxo","xooxo")
                case 'I' => Array("xxxo","oxoo","oxoo","oxoo","xxxo")
                case 'J' => Array("oooxo","oooxo","oooxo","xooxo","oxxoo")
                case 'K' => Array("xooxo","xoxoo","xxooo","xoxoo","xooxo")
                case 'L' => Array("xoooo","xoooo","xoooo","xoooo","xxxxo")
                case 'M' => Array("xooxo","xxxxo","xooxo","xooxo","xooxo")
                case 'N' => Array("xooxo","xxoxo","xoxxo","xooxo","xooxo")
                case 'O' => Array("oxxoo","xooxo","xooxo","xooxo","oxxoo")
                case 'P' => Array("xxxoo","xooxo","xxxoo","xoooo","xoooo")
                case 'Q' => Array("oxxoo","xooxo","xooxo","xoxxo","oxxxo")
                case 'R' => Array("xxxoo","xooxo","xxxoo","xoxoo","xooxo")
                case 'S' => Array("oxxxo","xoooo","oxxoo","oooxo","xxxoo")
                case 'T' => Array("xxxo","oxoo","oxoo","oxoo","oxoo")
                case 'U' => Array("xooxo","xooxo","xooxo","xooxo","oxxoo")
                case 'V' => Array("xoxo","xoxo","xoxo","xoxo","oxoo")
                case 'W' => Array("xooxo","xooxo","xooxo","xxxxo","xooxo")
                case 'X' => Array("xooxo","xooxo","oxxoo","xooxo","xooxo")
                case 'Y' => Array("xoxo","xoxo","oxoo","oxoo","oxoo")
                case 'Z' => Array("xxxxo","oooxo","oxxoo","xoooo","xxxxo")
                case ' ' => Array("oo","oo","oo","oo","oo")
                case '.' => Array("ooooo","ooooo","ooooo","ooooo","xoooo")
                case ',' => Array("ooooo","ooooo","ooooo","oxooo","xoooo")
                case '"' => Array("xxo","xxo","ooo","ooo","ooo")
                case '\'' => Array("xo","xo","oo","oo","oo")
                case '?' => Array("xxoooo","ooxooo","oxoooo","oooooo","oxoooo")
                case '!' => Array("xoooo","xoooo","xoooo","ooooo","xoooo")
                case ':' => Array("oo","xo","oo","xo","oo")
                case ';' => Array("oo","xo","oo","xo","xo")
                case '/' => Array("ooxo","ooxo","oxoo","xooo","xooo")
                case '_' => Array("oooo","oooo","oooo","oooo","xxxo")
                case '-' => Array("oooo","oooo","xxxo","oooo","oooo")
            }
        }


        def mkFlowString(input: String) : Array[String] = {
            var flowString = new Array[String](5) // 5 long strings, each representing a row

            var inputChar = input.toArray //toArray of Char
            var length = inputChar.length

            for (i <- 0 until length) {inputChar(i) = capitalize(inputChar(i))} //map Capitalize

            var inputArrayString = Array.ofDim[String](length+2,5)

            inputArrayString(0) = Array("ooooo","ooooo","ooooo","ooooo","ooooo")
            inputArrayString(length+1) = Array("ooooo","ooooo","ooooo","ooooo","ooooo")
            
            for (i <- 1 to length) {inputArrayString(i) = mkArrayString(inputChar(i-1))} //map mkArrayString, every cell is now the Array of 5 strings, plus 2 blank cells at the front and back.

            
            for (k <- 0 until 5) {
                var s: String = ""
                for (i <- 0 until length+2) {
                    s += inputArrayString(i)(k)
                }
                flowString(k) = s
            }
            flowString
        }

        flowString = mkFlowString(input)

        var length = flowString(0).length

        var flowChar = Array.ofDim[Char](5,length) // Array of Chars, where flowChar(0) is like a very long r1
        for (i <- 0 to 4) {
            flowChar(i) = flowString(i).toArray
        }

        /*
        for (k <- 0 to 4) {
            for (i <- 0 to length-1) {
                print(flowChar(k)(i))
            }
            println()
        }
        */
        // test passed: flowString and flowChar seems OK

        def toIntbool(char: Char) : Int = {
            // x -> 0; o -> 1
            if (char == 'x') 0
            else 1
        }

        // toHex takes a[i..i+3], and gives the hex as a String
        def toHex(a: Array[Int], i: Int) : String = {
            var sum: Int = a(i)*8 + a(i+1)*4 + a(i+2)*2 + a(i+3)
            sum match {
                case 10 => "a"
                case 11 => "b"
                case 12 => "c"
                case 13 => "d"
                case 14 => "e"
                case 15 => "f"
                case s: Int  => s.toString
            }
        }


        def makeHex(a: Array[Array[Char]], i: Int) : String = { // take a[i..i+4] and turns it into hex string
            // create sub arrays
            var r1 = new Array[Char](5)
            for (k <- 0 to 4) {
                r1(k) = a(0)(i+k)
            }
            //for (i <- 0 to 4) {print(r1(i).toString)}
            //println()

            var r2 = new Array[Char](5)
            for (k <- 0 to 4) {
                r2(k) = a(1)(i+k)
            }
            //for (i <- 0 to 4) {print(r1(i).toString)}
            //println()

            var r3 = new Array[Char](5)
            for (k <- 0 to 4) {
                r3(k) = a(2)(i+k)
            }

            var r4 = new Array[Char](5)
            for (k <- 0 to 4) {
                r4(k) = a(3)(i+k)
            }

            var r5 = new Array[Char](5)
            for (k <- 0 to 4) {
                r5(k) = a(4)(i+k)
            }

            var multiplex = Array.ofDim[Int](3,12)

            multiplex(0)(2) = 1
            multiplex(0)(11) = toIntbool(r1(0))
            multiplex(0)(10) = toIntbool(r1(2))
            multiplex(0)(9) = toIntbool(r1(4))
            multiplex(0)(8) = toIntbool(r4(4))
            multiplex(0)(7) = toIntbool(r4(3))
            multiplex(0)(6) = toIntbool(r4(2))
            multiplex(0)(5) = toIntbool(r4(1))
            multiplex(0)(4) = toIntbool(r4(0))
            multiplex(0)(3) = toIntbool(r3(1))

            multiplex(1)(1) = 1
            multiplex(1)(11) = toIntbool(r3(4))
            multiplex(1)(10) = toIntbool(r3(0))
            multiplex(1)(9) = toIntbool(r3(2))
            multiplex(1)(8) = toIntbool(r1(1))
            multiplex(1)(7) = toIntbool(r1(3))
            multiplex(1)(6) = toIntbool(r5(3))
            multiplex(1)(5) = toIntbool(r5(1))
            multiplex(1)(4) = 1
            multiplex(1)(3) = 1

            multiplex(2)(0) = 1
            multiplex(2)(11) = toIntbool(r5(2))
            multiplex(2)(10) = toIntbool(r5(4))
            multiplex(2)(9) = toIntbool(r5(0))
            multiplex(2)(8) = toIntbool(r2(0))
            multiplex(2)(7) = toIntbool(r2(1))
            multiplex(2)(6) = toIntbool(r2(2))
            multiplex(2)(5) = toIntbool(r2(3))
            multiplex(2)(4) = toIntbool(r2(4))
            multiplex(2)(3) = toIntbool(r3(3))
            
            val s1: String = toHex(multiplex(0),0)+toHex(multiplex(0),4)+toHex(multiplex(0),8)+"0"
            val s2: String = toHex(multiplex(1),0)+toHex(multiplex(1),4)+toHex(multiplex(1),8)+"0"
            val s3: String = toHex(multiplex(2),0)+toHex(multiplex(2),4)+toHex(multiplex(2),8)+"0"

            val output: String = "0x"+s1+", 0x"+s2+", 0x"+s3
            output
        }

        var content: String = ""

        //Pre data:
        content += "#include \"hardware.h\"\n\n"
        content += "#define TICK 5\n\n"

        // statics
        for (i <- 0 to length-5) {
            var hex: String = makeHex(flowChar, i)
            content += "static const unsigned image"+i.toString+"[] = { "+hex+" };"+"\n\n"
        }

        // frames
        content += "static const unsigned *frame[] = {\n"
        for (i <- 0 to length-6) {
            content += "image"+i.toString+", "
        }
        content += "image"+(length-5).toString+"\n"
        content += "};\n\n"

        // duration
        content += "static const int dur[] = {\n"
        for (i <- 0 to length-6) {
            content += "7, "
        }
        content += "7\n"
        content += "};\n\n"

        // Post data
        content += "static unsigned i = 0;\n"
        content += "static unsigned j = 0;\n"
        content += "static unsigned k = 0;\n"

        content += "void advance(void) {k++;if (k == 3) {k = 0; j++;if (j == dur[i]) {j = 0; i++;if (i == "+(length-4).toString+") i = 0;}};GPIO_OUT = frame[i][k];}\n\n"

        content += "void timer1_handler(void) {if (TIMER1_COMPARE[0]) {advance();TIMER1_COMPARE[0] = 0;}}\n\n"

        content += "void init_timer(void) {TIMER1_STOP = 1;TIMER1_MODE = TIMER_Mode_Timer;TIMER1_BITMODE = TIMER_16Bit;TIMER1_PRESCALER = 4;TIMER1_CLEAR = 1;TIMER1_CC[0] = 1000 * TICK;TIMER1_SHORTS = BIT(TIMER_COMPARE0_CLEAR);TIMER1_INTENSET = BIT(TIMER_INT_COMPARE0);TIMER1_START = 1;enable_irq(TIMER1_IRQ);}\n\n"

        content += "void init(void) {GPIO_DIR = 0xfff0;GPIO_PINCNF[BUTTON_A] = 0;GPIO_PINCNF[BUTTON_B] = 0;init_timer();while (1) {pause();}}\n\n"



        new PrintWriter("s2c_output") { write(content); close }
        println("done!")
    }
}
