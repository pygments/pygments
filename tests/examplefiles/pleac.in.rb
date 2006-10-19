# -*- ruby -*-

# Local variables:
#  indent-tabs-mode: nil
#  ruby-indent-level: 4
# End:

# @@PLEAC@@_NAME
# @@SKIP@@ Ruby

# @@PLEAC@@_WEB
# @@SKIP@@ http://www.ruby-lang.org


# @@PLEAC@@_1.0
string = '\n'                     # two characters, \ and an n
string = 'Jon \'Maddog\' Orwant'  # literal single quotes

string = "\n"                     # a "newline" character
string = "Jon \"Maddog\" Orwant"  # literal double quotes

string = %q/Jon 'Maddog' Orwant/  # literal single quotes

string = %q[Jon 'Maddog' Orwant]  # literal single quotes
string = %q{Jon 'Maddog' Orwant}  # literal single quotes
string = %q(Jon 'Maddog' Orwant)  # literal single quotes
string = %q<Jon 'Maddog' Orwant>  # literal single quotes

a = <<"EOF"
This is a multiline here document
terminated by EOF on a line by itself
EOF


# @@PLEAC@@_1.1
value = string[offset,count]
value = string[offset..-1]

string[offset,count] = newstring
string[offset..-1]   = newtail

# in Ruby we can also specify intervals by their two offsets
value = string[offset..offs2]
string[offset..offs2] = newstring

leading, s1, s2, trailing = data.unpack("A5 x3 A8 A8 A*")

fivers = string.unpack("A5" * (string.length/5))

chars = string.unpack("A1" * string.length)

string = "This is what you have"
#        +012345678901234567890  Indexing forwards  (left to right)
#         109876543210987654321- Indexing backwards (right to left)
#          note that 0 means 10 or 20, etc. above

first  = string[0, 1]       # "T"
start  = string[5, 2]       # "is"
rest   = string[13..-1]     # "you have"
last   = string[-1, 1]      # "e"
end_   = string[-4..-1]     # "have"
piece  = string[-8, 3]      # "you"

string[5, 2] = "wasn't"     # change "is" to "wasn't"
string[-12..-1] = "ondrous" # "This wasn't wondrous"
string[0, 1] = ""           # delete first character
string[-10..-1]  = ""       # delete last 10 characters

if string[-10..-1] =~ /pattern/
    puts "Pattern matches in last 10 characters"
end

string[0, 5].gsub!(/is/, 'at')

a = "make a hat"
a[0, 1], a[-1, 1] = a[-1, 1], a[0, 1]

a = "To be or not to be"
b = a.unpack("x6 A6")

b, c = a.unpack("x6 A2 X5 A2")
puts "#{b}\n#{c}\n"

def cut2fmt(*args)
    template = ''
    lastpos  = 1
    for place in args
        template += "A" + (place - lastpos).to_s + " "
        lastpos   = place
    end
    template += "A*"
    return template
end

fmt = cut2fmt(8, 14, 20, 26, 30)


# @@PLEAC@@_1.2
# careful! "b is true" doesn't mean "b != 0" (0 is true in Ruby)
# thus no problem of "defined" later since only nil is false
# the following sets to `c' if `b' is nil or false
a = b || c

# if you need Perl's behaviour (setting to `c' if `b' is 0) the most
# effective way is to use Numeric#nonzero? (thanks to Dave Thomas!)
a = b.nonzero? || c

# you will still want to use defined? in order to test
# for scope existence of a given object
a = defined?(b) ? b : c

dir = ARGV.shift || "/tmp"


# @@PLEAC@@_1.3
v1, v2 = v2, v1

alpha, beta, production = %w(January March August)
alpha, beta, production = beta, production, alpha


# @@PLEAC@@_1.4
num = char[0]
char = num.chr

# Ruby also supports having a char from character constant
num = ?r

char = sprintf("%c", num)
printf("Number %d is character %c\n", num, num)

ascii = string.unpack("C*")
string = ascii.pack("C*")

hal = "HAL"
ascii = hal.unpack("C*")
# We can't use Array#each since we can't mutate a Fixnum
ascii.collect! { |i|
    i + 1                         # add one to each ASCII value
}                
ibm = ascii.pack("C*")
puts ibm


# @@PLEAC@@_1.5
array = string.split('')

array = string.unpack("C*")

string.scan(/./) { |b|
    # do something with b
}

string = "an apple a day"
print "unique chars are: ", string.split('').uniq.sort, "\n"

sum = 0
for ascval in string.unpack("C*") # or use Array#each for a pure OO style :)
    sum += ascval
end
puts "sum is #{sum & 0xffffffff}" # since Ruby will go Bignum if necessary

# @@INCLUDE@@ include/ruby/slowcat.rb


# @@PLEAC@@_1.6
revbytes = string.reverse

revwords = string.split(" ").reverse.join(" ")

revwords = string.split(/(\s+)/).reverse.join

# using the fact that IO is Enumerable, you can directly "select" it
long_palindromes = File.open("/usr/share/dict/words").
    select { |w| w.chomp!; w.reverse == w && w.length > 5 }


# @@PLEAC@@_1.7
while string.sub!("\t+") { ' ' * ($&.length * 8 - $`.length % 8) }
end


# @@PLEAC@@_1.8
'You owe #{debt} to me'.gsub(/\#{(\w+)}/) { eval($1) }

rows, cols = 24, 80
text = %q(I am #{rows} high and #{cols} long)
text.gsub!(/\#{(\w+)}/) { eval("#{$1}") }
puts text

'I am 17 years old'.gsub(/\d+/) { 2 * $&.to_i }


# @@PLEAC@@_1.9
e = "bo peep".upcase
e.downcase!
e.capitalize!

"thIS is a loNG liNE".gsub!(/\w+/) { $&.capitalize }


# @@PLEAC@@_1.10
"I have #{n+1} guanacos."
print "I have ", n+1, " guanacos."


# @@PLEAC@@_1.11
var = <<'EOF'.gsub(/^\s+/, '')
    your text
    goes here
EOF


# @@PLEAC@@_1.12
string = "Folding and splicing is the work of an editor,\n"+
    "not a mere collection of silicon\n"+ 
    "and\n"+
    "mobile electrons!"

def wrap(str, max_size)
    all = []
    line = ''
    for l in str.split
        if (line+l).length >= max_size
            all.push(line)
            line = ''
        end
        line += line == '' ? l : ' ' + l
    end
    all.push(line).join("\n")
end

print wrap(string, 20)
#=> Folding and 
#=> splicing is the 
#=> work of an editor, 
#=> not a mere 
#=> collection of 
#=> silicon and mobile 
#=> electrons!


# @@PLEAC@@_1.13
string = %q(Mom said, "Don't do that.")
string.gsub(/['"]/) { '\\'+$& }
string.gsub(/['"]/, '\&\&')
string.gsub(/[^A-Z]/) { '\\'+$& }
"is a test!".gsub(/\W/) { '\\'+$& }  # no function like quotemeta?


# @@PLEAC@@_1.14
string.strip!


# @@PLEAC@@_1.15
def parse_csv(text)
    new = text.scan(/"([^\"\\]*(?:\\.[^\"\\]*)*)",?|([^,]+),?|,/)
    new << nil if text[-1] == ?,
    new.flatten.compact
end  

line = %q<XYZZY,"","O'Reilly, Inc","Wall, Larry","a \"glug\" bit,",5,"Error, Core Dumped">
fields = parse_csv(line)
fields.each_with_index { |v,i|
    print "#{i} : #{v}\n";
}


# @@PLEAC@@_1.16
# Use the soundex.rb Library from Michael Neumann.
# http://www.s-direktnet.de/homepages/neumann/rb_prgs/Soundex.rb
require 'Soundex'

code = Text::Soundex.soundex(string)
codes = Text::Soundex.soundex(array)

# substitution function for getpwent():
# returns an array of user entries,
# each entry contains the username and the full name
def login_names
    result = []
    File.open("/etc/passwd") { |file|
        file.each_line { |line|
            next if line.match(/^#/)
            cols = line.split(":")
            result.push([cols[0], cols[4]])
        }
    }
    result
end

puts "Lookup user: "
user = STDIN.gets
user.chomp!
exit unless user
name_code = Text::Soundex.soundex(user)

splitter = Regexp.new('(\w+)[^,]*\b(\w+)')
for username, fullname in login_names do
    firstname, lastname = splitter.match(fullname)[1,2]
    if name_code == Text::Soundex.soundex(username)
        || name_code == Text::Soundex.soundex(firstname)
        || name_code == Text::Soundex.soundex(lastname)
    then
        puts "#{username}: #{firstname} #{lastname}"
    end
end


# @@PLEAC@@_1.17
# @@INCLUDE@@ include/ruby/fixstyle.rb


# @@PLEAC@@_1.18
# @@INCLUDE@@ include/ruby/psgrep.rb


# @@PLEAC@@_2.1
# Matz tells that you can use Integer() for strict checked conversion.
Integer("abc")
#=> `Integer': invalid value for Integer: "abc" (ArgumentError)
Integer("567")
#=> 567

# You may use Float() for floating point stuff
Integer("56.7")
#=> `Integer': invalid value for Integer: "56.7" (ArgumentError)
Float("56.7")
#=> 56.7

# You may also use a regexp for that
if string =~ /^[+-]?\d+$/
    p 'is an integer'
else
    p 'is not'
end

if string =~ /^-?(?:\d+(?:\.\d*)?|\.\d+)$/
    p 'is a decimal number'
else
    p 'is not'
end


# @@PLEAC@@_2.2
# equal(num1, num2, accuracy) : returns true if num1 and num2 are
#   equal to accuracy number of decimal places
def equal(i, j, a)
    sprintf("%.#{a}g", i) == sprintf("%.#{a}g", j)
end

wage = 536                        # $5.36/hour
week = 40 * wage                  # $214.40
printf("One week's wage is: \$%.2f\n", week/100.0)


# @@PLEAC@@_2.3
num.round                         # rounds to integer

a = 0.255
b = sprintf("%.2f", a)
print  "Unrounded: #{a}\nRounded: #{b}\n"
printf "Unrounded: #{a}\nRounded: %.2f\n", a

print "number\tint\tfloor\tceil\n"
a = [ 3.3 , 3.5 , 3.7, -3.3 ]
for n in a
    printf("% .1f\t% .1f\t% .1f\t% .1f\n",  # at least I don't fake my output :)
           n, n.to_i, n.floor, n.ceil)
end


# @@PLEAC@@_2.4
def dec2bin(n)
    [n].pack("N").unpack("B32")[0].sub(/^0+(?=\d)/, '')
end

def bin2dec(n)
    [("0"*32+n.to_s)[-32..-1]].pack("B32").unpack("N")[0]
end


# @@PLEAC@@_2.5
for i in x .. y
    # i is set to every integer from x to y, inclusive
end

x.step(y,7) { |i|
    # i is set to every integer from x to y, stepsize = 7
}

print "Infancy is: "
(0..2).each { |i|
    print i, " "
}
print "\n"


# @@PLEAC@@_2.6
# We can add conversion methods to the Integer class,
# this makes a roman number just a representation for normal numbers.
class Integer
    
    @@romanlist = [["M", 1000],
                   ["CM", 900],
                   ["D",  500],
                   ["CD", 400],
                   ["C",  100],
                   ["XC",  90],
                   ["L",   50],
                   ["XL",  40],
                   ["X",   10],
                   ["IX",   9],
                   ["V",    5],
                   ["IV",   4],
                   ["I",    1]]
    
    def to_roman
        remains = self
        roman = ""
        for sym, num in @@romanlist
            while remains >= num
                remains -= num
                roman << sym
            end
        end
        roman
    end
    
    def Integer.from_roman(roman)
        ustr = roman.upcase
        sum = 0
        for entry in @@romanlist
            sym, num = entry[0], entry[1]
            while sym == ustr[0, sym.length]
                sum += num
                ustr.slice!(0, sym.length)
            end
        end
        sum
    end
    
end


roman_fifteen = 15.to_roman
puts "Roman for fifteen is #{roman_fifteen}"
i = Integer.from_roman(roman_fifteen)
puts "Converted back, #{roman_fifteen} is #{i}"

# check
for i in (1..3900)
    r = i.to_roman
    j = Integer.from_roman(r)
    if i != j
        puts "error: #{i} : #{r} - #{j}"
    end
end


# @@PLEAC@@_2.7
random = rand(y-x+1)+x

chars = ["A".."Z","a".."z","0".."9"].collect { |r| r.to_a }.join + %q(!@$%^&*)
password = (1..8).collect { chars[rand(chars.size)] }.pack("C*")


# @@PLEAC@@_2.8
srand        # uses a combination of the time, the process id, and a sequence number
srand(val)   # for repeatable behaviour


# @@PLEAC@@_2.9
# from the randomr lib: 
# http://raa.ruby-lang.org/project/randomr/
----> http://raa.ruby-lang.org/project/randomr/

require 'random/mersenne_twister'
mers = Random::MersenneTwister.new 123456789
puts mers.rand(0)    # 0.550321932544541
puts mers.rand(10)   # 2

# using online sources of random data via the realrand package:
# http://raa.ruby-lang.org/project/realrand/
# **Note**
# The following online services are used in this package:
#   http://www.random.org - source: atmospheric noise 
#   http://www.fourmilab.ch/hotbits - source: radioactive decay timings
#   http://random.hd.org - source: entropy from local and network noise
# Please visit the sites and respect the rules of each service.

require 'random/online'

generator1 = Random::RandomOrg.new
puts generator1.randbyte(5).join(",")
puts generator1.randnum(10, 1, 6).join(",")  # Roll dice 10 times.

generator2 = Random::FourmiLab.new
puts generator2.randbyte(5).join(",")
# randnum is not supported.

generator3 = Random::EntropyPool.new
puts generator3.randbyte(5).join(",")
# randnum is not supported.


# @@PLEAC@@_2.10
def gaussian_rand
    begin
        u1 = 2 * rand() - 1
        u2 = 2 * rand() - 1
        w = u1*u1 + u2*u2
    end while (w >= 1)
    w = Math.sqrt((-2*Math.log(w))/w)
    [ u2*w, u1*w ]
end

mean = 25
sdev = 2
salary = gaussian_rand[0] * sdev + mean
printf("You have been hired at \$%.2f\n", salary)


# @@PLEAC@@_2.11
def deg2rad(d)
    (d/180.0)*Math::PI
end

def rad2deg(r)
    (r/Math::PI)*180
end


# @@PLEAC@@_2.12
sin_val = Math.sin(angle)
cos_val = Math.cos(angle)
tan_val = Math.tan(angle)

# AFAIK Ruby's Math module doesn't provide acos/asin
# While we're at it, let's also define missing hyperbolic functions
module Math
    def Math.asin(x)
        atan2(x, sqrt(1 - x**2))
    end
    def Math.acos(x)
        atan2(sqrt(1 - x**2), x)
    end
    def Math.atan(x)
        atan2(x, 1)
    end
    def Math.sinh(x)
        (exp(x) - exp(-x)) / 2
    end
    def Math.cosh(x)
        (exp(x) + exp(-x)) / 2
    end
    def Math.tanh(x)
        sinh(x) / cosh(x)
    end
end

# The support for Complex numbers is not built-in
y = Math.acos(3.7)
#=> in `sqrt': square root for negative number (ArgumentError)

# There is an implementation of Complex numbers in 'complex.rb' in current
# Ruby distro, but it doesn't support atan2 with complex args, so it doesn't
# solve this problem.


# @@PLEAC@@_2.13
log_e = Math.log(val)
log_10 = Math.log10(val)

def log_base(base, val)
    Math.log(val)/Math.log(base)
end

answer = log_base(10, 10_000)
puts "log10(10,000) = #{answer}"


# @@PLEAC@@_2.14
require 'matrix.rb'

a = Matrix[[3, 2, 3], [5, 9, 8]]
b = Matrix[[4, 7], [9, 3], [8, 1]]
c = a * b

a.row_size
a.column_size

c.det
a.transpose


# @@PLEAC@@_2.15
require 'complex.rb'
require 'rational.rb'

a = Complex(3, 5)              # 3 + 5i
b = Complex(2, -2)             # 2 - 2i
puts "c = #{a*b}"

c = a * b
d = 3 + 4*Complex::I

printf "sqrt(#{d}) = %s\n", Math.sqrt(d)


# @@PLEAC@@_2.16
number = hexadecimal.hex
number = octal.oct

print "Gimme a number in decimal, octal, or hex: "
num = gets.chomp
exit unless defined?(num)
num = num.oct if num =~ /^0/  # does both oct and hex  
printf "%d %x %o\n", num, num, num

print "Enter file permission in octal: "
permissions = gets.chomp
raise "Exiting ...\n" unless defined?(permissions)
puts "The decimal value is #{permissions.oct}"


# @@PLEAC@@_2.17
def commify(n)
    n.to_s =~ /([^\.]*)(\..*)?/
    int, dec = $1.reverse, $2 ? $2 : ""
    while int.gsub!(/(,|\.|^)(\d{3})(\d)/, '\1\2,\3')
    end
    int.reverse + dec
end


# @@PLEAC@@_2.18
printf "It took %d hour%s\n", time, time == 1 ? "" : "s"

# dunno if an equivalent to Lingua::EN::Inflect exists...


# @@PLEAC@@_2.19
#-----------------------------
#!/usr/bin/ruby
# bigfact - calculating prime factors
def factorize(orig)
    factors = {}
    factors.default = 0     # return 0 instead nil if key not found in hash
    n = orig
    i = 2
    sqi = 4                 # square of i
    while sqi <= n do
        while n.modulo(i) == 0 do
            n /= i
            factors[i] += 1
            # puts "Found factor #{i}"
        end
        # we take advantage of the fact that (i +1)**2 = i**2 + 2*i +1
        sqi += 2 * i + 1
        i += 1
    end
    
    if (n != 1) && (n != orig)
        factors[n] += 1
    end
    factors
end

def printfactorhash(orig, factorcount)
    print format("%-10d ", orig)
    if factorcount.length == 0
        print "PRIME"
    else
        # sorts after number, because the hash keys are numbers
        factorcount.sort.each { |factor,exponent|
            print factor
            if exponent > 1
                print "**", exponent
            end
            print " "
        }
    end
    puts
end

for arg in ARGV
    n = arg.to_i
    mfactors = factorize(n)
    printfactorhash(n, mfactors)
end
#-----------------------------


# @@PLEAC@@_3.0
puts Time.now

print "Today is day ", Time.now.yday, " of the current year.\n"
print "Today is day ", Time.now.day, " of the current month.\n"


# @@PLEAC@@_3.1
day, month, year = Time.now.day, Time.now.month, Time.now.year
# or
day, month, year = Time.now.to_a[3..5]

tl = Time.now.localtime
printf("The current date is %04d %02d %02d\n", tl.year, tl.month, tl.day)

Time.now.localtime.strftime("%Y-%m-%d")


# @@PLEAC@@_3.2
Time.local(year, month, day, hour, minute, second).tv_sec
Time.gm(year, month, day, hour, minute, second).tv_sec


# @@PLEAC@@_3.3
sec, min, hour, day, month, year, wday, yday, isdst, zone = Time.at(epoch_secs).to_a


# @@PLEAC@@_3.4
when_ = now + difference         # now -> Time ; difference -> Numeric (delta in seconds)
then_ = now - difference


# @@PLEAC@@_3.5
bree = 361535725
nat  =  96201950

difference = bree - nat
puts "There were #{difference} seconds between Nat and Bree"

seconds    =  difference % 60
difference = (difference - seconds) / 60
minutes    =  difference % 60
difference = (difference - minutes) / 60
hours      =  difference % 24
difference = (difference - hours)   / 24
days       =  difference % 7
weeks      = (difference - days)    /  7

puts "(#{weeks} weeks, #{days} days, #{hours}:#{minutes}:#{seconds})"


# @@PLEAC@@_3.6
monthday, weekday, yearday = date.mday, date.wday, date.yday

# AFAIK the week number is not just a division since week boundaries are on sundays
weeknum = d.strftime("%U").to_i + 1

year  = 1981
month = "jun"                     # or `6' if you want to emulate a broken language
day   = 16
t = Time.mktime(year, month, day)
print "#{month}/#{day}/#{year} was a ", t.strftime("%A"), "\n"


# @@PLEAC@@_3.7
yyyy, mm, dd = $1, $2, $3 if "1998-06-25" =~ /(\d+)-(\d+)-(\d+)/

epoch_seconds = Time.mktime(yyyy, mm, dd).tv_sec

# dunno an equivalent to Date::Manip#ParseDate


# @@PLEAC@@_3.8
string = Time.at(epoch_secs)
Time.at(1234567890).gmtime        # gives: Fri Feb 13 23:31:30 UTC 2009

time = Time.mktime(1973, "jan", 18, 3, 45, 50)
print "In localtime it gives: ", time.localtime, "\n"


# @@PLEAC@@_3.9
# Ruby provides micro-seconds in Time object
Time.now.usec

# Ruby gives the seconds in floating format when substracting two Time objects
before = Time.now
line = gets
elapsed = Time.now - before
puts "You took #{elapsed} seconds."

# On my Celeron-400 with Linux-2.2.19-14mdk, average for three execs are:
#   This Ruby version:       average 0.00321 sec
#   Cookbook's Perl version: average 0.00981 sec
size = 500
number_of_times = 100
total_time = 0
number_of_times.times {
    # populate array
    array = []
    size.times { array << rand }
    # sort it
    begin_ = Time.now
    array.sort!
    time = Time.now - begin_
    total_time += time
}
printf "On average, sorting %d random numbers takes %.5f seconds\n",
    size, (total_time/Float(number_of_times))


# @@PLEAC@@_3.10
sleep(0.005)                      # Ruby is definitely not as broken as Perl :)
# (may be interrupted by sending the process a SIGALRM)


# @@PLEAC@@_3.11
#!/usr/bin/ruby -w
# hopdelta - feed mail header, produce lines
#            showing delay at each hop.
require 'time'
class MailHopDelta

    def initialize(mail)
        @head = mail.gsub(/\n\s+/,' ')
        @topline = %w-Sender Recipient Time Delta-
        @start_from = mail.match(/^From.*\@([^\s>]*)/)[1]
        @date = Time.parse(mail.match(/^Date:\s+(.*)/)[1])
    end

    def out(line)
         "%-20.20s %-20.20s %-20.20s  %s" % line
    end

    def hop_date(day)
        day.strftime("%I:%M:%S %Y/%m/%d")
    end

    def puts_hops
        puts out(@topline) 
        puts out(['Start', @start_from, hop_date(@date),''])
        @head.split(/\n/).reverse.grep(/^Received:/).each do |hop|
            hop.gsub!(/\bon (.*?) (id.*)/,'; \1')
            whence = hop.match(/;\s+(.*)$/)[1]
            unless whence
                warn "Bad received line: #{hop}"
                next
            end
            from = $+ if hop =~ /from\s+(\S+)|\((.*?)\)/
            by   = $1 if hop =~ /by\s+(\S+\.\S+)/
            next unless now = Time.parse(whence).localtime
            delta = now - @date
            puts out([from, by, hop_date(now), hop_time(delta)])
            @date = now
        end
    end

    def hop_time(secs)
        sign = secs < 0 ? -1 : 1
        days, secs = secs.abs.divmod(60 * 60 * 24)
        hours,secs = secs.abs.divmod(60 * 60)
        mins, secs = secs.abs.divmod(60)
        rtn =  "%3ds" % [secs  * sign]
        rtn << "%3dm" % [mins  * sign] if mins  != 0
        rtn << "%3dh" % [hours * sign] if hours != 0
        rtn << "%3dd" % [days  * sign] if days  != 0 
        rtn
    end
end

$/ = ""
mail = MailHopDelta.new(ARGF.gets).puts_hops


# @@PLEAC@@_4.0
single_level = [ "this", "that", "the", "other" ]

# Ruby directly supports nested arrays
double_level = [ "this", "that", [ "the", "other" ] ]
still_single_level = [ "this", "that", [ "the", "other" ] ].flatten


# @@PLEAC@@_4.1
a = [ "quick", "brown", "fox" ]
a = %w(Why are you teasing me?)

lines = <<"END_OF_HERE_DOC".gsub(/^\s*(.+)/, '\1')
    The boy stood on the burning deck,
    It was as hot as glass.
END_OF_HERE_DOC

bigarray = IO.readlines("mydatafile").collect { |l| l.chomp }

name = "Gandalf"
banner = %Q(Speak, #{name}, and welcome!)

host_info  = `host #{his_host}`

%x(ps #{$$})

banner = 'Costs only $4.95'.split(' ')

rax = %w! ( ) < > { } [ ] !


# @@PLEAC@@_4.2
def commify_series(arr)
    return '' if not arr
    case arr.size
        when 0 then ''
        when 1 then arr[0]
        when 2 then arr.join(' and ')
        else arr[0..-2].join(', ') + ', and ' + arr[-1]
    end
end

array = [ "red", "yellow", "green" ]

print "I have ", array, " marbles\n"
# -> I have redyellowgreen marbles

# But unlike Perl:
print "I have #{array} marbles\n"
# -> I have redyellowgreen marbles
# So, needs:
print "I have #{array.join(' ')} marbles\n"
# -> I have red yellow green marbles

#!/usr/bin/ruby
# communify_series - show proper comma insertion in list output

def commify_series(arr)
    return '' if not arr
    sepchar = arr.find { |p| p =~ /,/ } ? '; ' : ', '
    case arr.size
        when 0 then ''
        when 1 then arr[0]
        when 2 then arr.join(' and ')
        else arr[0..-2].join(sepchar) + sepchar + 'and ' + arr[-1]
    end
end

lists = [
    [ 'just one thing' ],
    %w(Mutt Jeff),
    %w(Peter Paul Mary),
    [ 'To our parents', 'Mother Theresa', 'God' ],
    [ 'pastrami', 'ham and cheese', 'peanut butter and jelly', 'tuna' ],
    [ 'recycle tired, old phrases', 'ponder big, happy thoughts' ],
    [ 'recycle tired, old phrases',
      'ponder big, happy thoughts',
      'sleep and dream peacefully' ],
]

for list in lists do
    puts "The list is: #{commify_series(list)}."
end


# @@PLEAC@@_4.3
#   (note: AFAIK Ruby doesn't allow gory change of Array length)
# grow the array by assigning nil to past the end of array
ary[new_size-1] = nil
# shrink the array by slicing it down
ary.slice!(new_size..-1)
# init the array with given size
Array.new(number_of_elems)
# assign to an element past the original end enlarges the array
ary[index_new_last_elem] = value

def what_about_that_array(a)
    print "The array now has ", a.size, " elements.\n"
    # Index of last element is not really interesting in Ruby
    print "Element #3 is `#{a[3]}'.\n"
end
people = %w(Crosby Stills Nash Young)
what_about_that_array(people)


# @@PLEAC@@_4.4
# OO style
bad_users.each { |user|
    complain(user)
}
# or, functional style
for user in bad_users
    complain(user)
end

for var in ENV.keys.sort
    puts "#{var}=#{ENV[var]}"
end

for user in all_users
    disk_space = get_usage(user)
    if (disk_space > MAX_QUOTA)
        complain(user)
    end
end

for l in IO.popen("who").readlines
    print l if l =~ /^gc/ 
end

# we can mimic the obfuscated Perl way
while fh.gets               # $_ is set to the line just read
    chomp                   # $_ has a trailing \n removed, if it had one
    split.each { |w|        # $_ is split on whitespace
                            # but $_ is not set to each chunk as in Perl
        print w.reverse
    }
end
# ...or use a cleaner way
for l in fh.readlines
    l.chomp.split.each { |w| print w.reverse }
end

# same drawback as in problem 1.4, we can't mutate a Numeric...
array.collect! { |v| v - 1 }

a = [ .5, 3 ]; b = [ 0, 1 ]
for ary in [ a, b ]
    ary.collect! { |v| v * 7 }
end
puts "#{a.join(' ')} #{b.join(' ')}"

# we can mutate Strings, cool; we need a trick for the scalar
for ary in [ [ scalar ], array, hash.values ]
    ary.each { |v| v.strip! }     # String#strip rules :)
end


# @@PLEAC@@_4.5
# not relevant in Ruby since we have always references
for item in array
    # do somethingh with item
end


# @@PLEAC@@_4.6
unique = list.uniq

# generate a list of users logged in, removing duplicates
users = `who`.collect { |l| l =~ /(\w+)/; $1 }.sort.uniq
puts("users logged in: #{commify_series(users)}")  # see 4.2 for commify_series


# @@PLEAC@@_4.7
a - b
# [ 1, 1, 2, 2, 3, 3, 3, 4, 5 ] - [ 1, 2, 4 ]  ->  [3, 5]


# @@PLEAC@@_4.8
union = a | b
intersection = a & b
difference = a - b


# @@PLEAC@@_4.9
array1.concat(array2)
# if you will assign to another object, better use:
new_ary = array1 + array2

members = [ "Time", "Flies" ]
initiates =  [ "An", "Arrow" ]
members += initiates

members = [ "Time", "Flies" ]
initiates = [ "An", "Arrow" ]
members[2,0] = [ "Like", initiates ].flatten

members[0] = "Fruit"
members[3,2] = "A", "Banana"


# @@PLEAC@@_4.10
reversed = ary.reverse

ary.reverse_each { |e|
    # do something with e
}

descending = ary.sort.reverse
descending = ary.sort { |a,b| b <=> a }


# @@PLEAC@@_4.11
# remove n elements from front of ary (shift n)
front = ary.slice!(0, n)

# remove n elements from the end of ary (pop n)
end_ = ary.slice!(-n .. -1)

# let's extend the Array class, to make that useful
class Array
    def shift2()
        slice!(0 .. 1)     # more symetric with pop2...
    end
    def pop2()
        slice!(-2 .. -1)
    end
end

friends = %w(Peter Paul Mary Jim Tim)
this, that = friends.shift2

beverages = %w(Dew Jolt Cola Sprite Fresca)
pair = beverages.pop2


# @@PLEAC@@_4.12
# use Enumerable#detect (or the synonym Enumerable#find)
highest_eng = employees.detect { |emp| emp.category == 'engineer' }


# @@PLEAC@@_4.13
# use Enumerable#select (or the synonym Enumerable#find_all)
bigs = nums.select { |i| i > 1_000_000 }
pigs = users.keys.select { |k| users[k] > 1e7 }

matching = `who`.select { |u| u =~ /^gnat / }

engineers = employees.select { |e| e.position == 'Engineer' }

secondary_assistance = applicants.select { |a|
    a.income >= 26_000 && a.income < 30_000
}


# @@PLEAC@@_4.14
# normally you would have an array of Numeric (Float or
# Fixnum or Bignum), so you would use:
sorted = unsorted.sort
# if you have strings representing Integers or Floats
# you may specify another sort method:
sorted = unsorted.sort { |a,b| a.to_f <=> b.to_f }

# let's use the list of my own PID's
`ps ux`.split("\n")[1..-1].
    select { |i| i =~ /^#{ENV['USER']}/ }.
    collect { |i| i.split[1] }.
    sort { |a,b| a.to_i <=> b.to_i }.each { |i| puts i }
puts "Select a process ID to kill:"
pid = gets.chomp
raise "Exiting ... \n" unless pid && pid =~ /^\d+$/
Process.kill('TERM', pid.to_i)
sleep 2
Process.kill('KILL', pid.to_i)

descending = unsorted.sort { |a,b| b.to_f <=> a.to_f }


# @@PLEAC@@_4.15
ordered = unordered.sort { |a,b| compare(a,b) }

precomputed = unordered.collect { |e| [compute, e] }
ordered_precomputed = precomputed.sort { |a,b| a[0] <=> b[0] }
ordered = ordered_precomputed.collect { |e| e[1] }

ordered = unordered.collect { |e| [compute, e] }.
    sort { |a,b| a[0] <=> b[0] }.
    collect { |e| e[1] }

for employee in employees.sort { |a,b| a.name <=> b.name }
    print employee.name, " earns \$ ", employee.salary, "\n"
end

# Beware! `0' is true in Ruby.
# For chaining comparisons, you may use Numeric#nonzero?, which
# returns num if num is not zero, nil otherwise
sorted = employees.sort { |a,b| (a.name <=> b.name).nonzero? || b.age <=> a.age }

users = []
# getpwent is not wrapped in Ruby... let's fallback
IO.readlines('/etc/passwd').each { |u| users << u.split(':') }
users.sort! { |a,b| a[0] <=> b[0] }
for user in users
    puts user[0]
end

sorted = names.sort { |a,b| a[1, 1] <=> b[1, 1] }
sorted = strings.sort { |a,b| a.length <=> b.length }

# let's show only the compact version
ordered = strings.collect { |e| [e.length, e] }.
    sort { |a,b| a[0] <=> b[0] }.
    collect { |e| e[1] }

ordered = strings.collect { |e| [/\d+/.match(e)[0].to_i, e] }.
    sort { |a,b| a[0] <=> b[0] }.
    collect { |e| e[1] }

print `cat /etc/passwd`.collect { |e| [e, e.split(':').indexes(3,2,0)].flatten }.
    sort { |a,b| (a[1] <=> b[1]).nonzero? || (a[2] <=> b[2]).nonzero? || a[3] <=> b[3] }.
    collect { |e| e[0] }


# @@PLEAC@@_4.16
circular.unshift(circular.pop)        # the last shall be first
circular.push(circular.shift)         # and vice versa

def grab_and_rotate(l)
    l.push(ret = l.shift)
    ret
end

processes = [1, 2, 3, 4, 5]
while (1)
    process = grab_and_rotate(processes)
    puts "Handling process #{process}"
    sleep 1
end


# @@PLEAC@@_4.17
def fisher_yates_shuffle(a)
    (a.size-1).downto(1) { |i|
        j = rand(i+1)
        a[i], a[j] = a[j], a[i] if i != j
    }
end

def naive_shuffle(a)
    for i in 0...a.size
        j = rand(a.size)
        a[i], a[j] = a[j], a[i]
    end
end


# @@PLEAC@@_4.18
#!/usr/bin/env ruby
# example 4-2 words
# words - gather lines, present in colums

# class to encapsulate the word formatting from the input
class WordFormatter
    def initialize(cols)
        @cols = cols
    end

    # helper to return the length of the longest word in the wordlist
    def maxlen(wordlist)
        max = 1
        for word in wordlist
            if word.length > max
                max = word.length
            end
        end
        max
    end

    # process the wordlist and print it formmated into columns
    def output(wordlist)
        collen = maxlen(wordlist) + 1
        columns = @cols / collen
        columns = 1 if columns == 0
        rows = (wordlist.length + columns - 1) / columns
        # now process each item, picking out proper piece for this position
        0.upto(rows * columns - 1) { |item|
            target = (item % columns) * rows + (item / columns)
            eol = ((item+1) % columns == 0)
            piece = wordlist[target] || ""
            piece = piece.ljust(collen) unless eol
            print piece
            puts if eol
        }
        # no need to finish it up, because eol is always true for the last element
    end
end

# get nr of chars that fit in window or console, see PLEAC 15.4
# not portable -- linux only (?)
def getWinCharWidth()
    buf = "\0" * 8
    $stdout.ioctl(0x5413, buf)
    ws_row, ws_col, ws_xpixel, ws_ypixel = buf.unpack("$4")
    ws_col || 80
rescue
    80
end

# main program
cols = getWinCharWidth()
formatter = WordFormatter.new(cols)
words = readlines()
words.collect! { |line|
    line.chomp
}
formatter.output(words)


# @@PLEAC@@_4.19
# In ruby, Fixnum's are automatically converted to Bignum's when
# needed, so there is no need for an extra module
def factorial(n)
    s = 1
    while n > 0
        s *= n
        n -= 1
    end
    s
end

puts factorial(500)

#---------------------------------------------------------
# Example 4-3. tsc-permute
# tsc_permute: permute each word of input
def permute(items, perms)
    unless items.length > 0
        puts perms.join(" ")
    else
        for i in items
            newitems = items.dup
            newperms = perms.dup
            newperms.unshift(newitems.delete(i))
            permute(newitems, newperms)
        end
    end
end
# In ruby the main program must be after all definitions it is using
permute(ARGV, [])

#---------------------------------------------------------
# mjd_permute: permute each word of input

def factorial(n)
    s = 1
    while n > 0
        s *= n
        n -= 1
    end
    s
end

# we use a class with a class variable store the private cache
# for the results of the factorial function.
class Factorial
    @@fact = [ 1 ]
    def Factorial.compute(n)
        if @@fact[n]
            @@fact[n]
        else
            @@fact[n] = n * Factorial.compute(n - 1)
        end
    end
end

#---------------------------------------------------------
# Example 4-4- mjd-permute
# n2pat(n, len): produce the N-th pattern of length len

# We must use a lower case letter as parameter N, otherwise it is
# handled as constant Length is the length of the resulting
# array, not the index of the last element (length -1) like in
# the perl example.
def n2pat(n, length)
    pat = []
    i = 1
    while i <= length
        pat.push(n % i)
        n /= i
        i += 1
    end
    pat
end

# pat2perm(pat): turn pattern returned by n2pat() into
# permutation of integers.
def pat2perm(pat)
    source = (0 .. pat.length - 1).to_a
    perm = []
    perm.push(source.slice!(pat.pop)) while pat.length > 0
    perm
end

def n2perm(n, len)
    pat2perm(n2pat(n,len))
end

# In ruby the main program must be after all definitions
while gets
    data = split
    # the perl solution has used $#data, which is length-1
    num_permutations = Factorial.compute(data.length())
    0.upto(num_permutations - 1) do |i|
        # in ruby we can not use an array as selector for an array
        # but by exchanging the two arrays, we can use the collect method
        # which returns an array with the result of all block invocations
        permutation = n2perm(i, data.length).collect {
            |j| data[j]
        }
        puts permutation.join(" ")
    end
end


# @@PLEAC@@_5.0
age = { "Nat",   24,
        "Jules", 25,
        "Josh",  17  }

age["Nat"]   = 24
age["Jules"] = 25
age["Josh"]  = 17

food_color = {
    "Apple"  => "red",
    "Banana" => "yellow",
    "Lemon"  => "yellow",
    "Carrot" => "orange"
             }

# In Ruby, you cannot avoid the double or simple quoting
# while manipulatin hashes


# @@PLEAC@@_5.1
hash[key] = value

food_color["Raspberry"] = "pink"
puts "Known foods:", food_color.keys


# @@PLEAC@@_5.2
# does hash have a value for key ?
if (hash.has_key?(key))
    # it exists
else
    # it doesn't
end

[ "Banana", "Martini" ].each { |name|
    print name, " is a ", food_color.has_key?(name) ? "food" : "drink", "\n"
}

age = {}
age['Toddler'] = 3
age['Unborn'] = 0
age['Phantasm'] = nil

for thing in ['Toddler', 'Unborn', 'Phantasm', 'Relic']
    print "#{thing}: "
    print "Has-key " if age.has_key?(thing)
    print "True " if age[thing]
    print "Nonzero " if age[thing] && age[thing].nonzero?
    print "\n"
end

#=>
# Toddler: Has-key True Nonzero 
# Unborn: Has-key True 
# Phantasm: Has-key 
# Relic: 

# You use Hash#has_key? when you use Perl's exists -> it checks
# for existence of a key in a hash.
# All Numeric are "True" in ruby, so the test doesn't have the
# same semantics as in Perl; you would use Numeric#nonzero? to
# achieve the same semantics (false if 0, true otherwise).


# @@PLEAC@@_5.3
food_color.delete("Banana")


# @@PLEAC@@_5.4
hash.each { |key, value|
    # do something with key and value
}

hash.each_key { |key|
    # do something with key
}

food_color.each { |food, color|
    puts "#{food} is #{color}"
}

food_color.each_key { |food|
    puts "#{food} is #{food_color[food]}"
}

# IMO this demonstrates that OO style is by far more readable
food_color.keys.sort.each { |food|
    puts "#{food} is #{food_color[food]}."
}

#-----------------------------
#!/usr/bin/ruby
# countfrom - count number of messages from each sender

# Default value is 0
from = Hash.new(0)
while gets
    /^From: (.*)/ and from[$1] += 1
end

# More useful to sort by number of received mail by person
from.sort {|a,b| b[1]<=>a[1]}.each { |v|
    puts "#{v[1]}: #{v[0]}"
}
#-----------------------------


# @@PLEAC@@_5.5
# You may use the built-in 'inspect' method this way:
p hash

# Or do it the Cookbook way:
hash.each { |k,v| puts "#{k} => #{v}" }

# Sorted by keys
hash.sort.each { |e| puts "#{e[0]} => #{e[1]}" }
# Sorted by values
hash.sort{|a,b| a[1]<=>b[1]}.each { |e| puts "#{e[0]} => #{e[1]}" }


# @@PLEAC@@_5.7
ttys = Hash.new
for i in `who`
    user, tty = i.split
    (ttys[user] ||= []) << tty               # see problems_ruby for more infos
end
ttys.keys.sort.each { |k|
    puts "#{k}: #{commify_series(ttys[k])}"  # from 4.2
}


# @@PLEAC@@_5.8
surname = { "Mickey" => "Mantle", "Babe" => "Ruth" }
puts surname.index("Mantle")

# If you really needed to 'invert' the whole hash, use Hash#invert

#-----------------------------
#!/usr/bin/ruby -w
# foodfind - find match for food or color

given = ARGV.shift or raise "usage: foodfind food_or_color"

color = {
    "Apple"  => "red",
    "Banana" => "yellow",
    "Lemon"  => "yellow",
    "Carrot" => "orange",
}

if (color.has_key?(given))
    puts "#{given} is a food with color #{color[given]}."
end
if (color.has_value?(given))
    puts "#{color.index(given)} is a food with color #{given}."
end
#-----------------------------


# @@PLEAC@@_5.9
# Sorted by keys (Hash#sort gives an Array of pairs made of each key,value)
food_color.sort.each { |f|
    puts "#{f[0]} is #{f[1]}."
}

# Sorted by values
food_color.sort { |a,b| a[1] <=> b[1] }.each { |f|
    puts "#{f[0]} is #{f[1]}."
}

# Sorted by length of values
food_color.sort { |a,b| a[1].length <=> b[1].length }.each { |f|
    puts "#{f[0]} is #{f[1]}."
}


# @@PLEAC@@_5.10
merged = a.clone.update(b)        # because Hash#update changes object in place

drink_color = { "Galliano"  => "yellow", "Mai Tai" => "blue" }
ingested_color = drink_color.clone.update(food_color)

substance_color = {}
for i in [ food_color, drink_color ]
    i.each_key { |k|
        if substance_color.has_key?(k)
            puts "Warning: #{k} seen twice.  Using the first definition."
            next
        end
        substance_color[k] = 1
    }
end


# @@PLEAC@@_5.11
common = hash1.keys & hash2.keys

this_not_that = hash1.keys - hash2.keys


# @@PLEAC@@_5.12
# no problem here, Ruby handles any kind of object for key-ing
# (it takes Object#hash, which defaults to Object#id)


# @@PLEAC@@_5.13
# AFAIK, not possible in Ruby


# @@PLEAC@@_5.14
# Be careful, the following is possible only because Fixnum objects are
# special (documentation says: there is effectively only one Fixnum object
# instance for any given integer value).
count = Hash.new(0)
array.each { |e|
    count[e] += 1
}


# @@PLEAC@@_5.15
father = {
    "Cain"      , "Adam",
    "Abel"      , "Adam",
    "Seth"      , "Adam",
    "Enoch"     , "Cain",
    "Irad"      , "Enoch",
    "Mehujael"  , "Irad",
    "Methusael" , "Mehujael",
    "Lamech"    , "Methusael",
    "Jabal"     , "Lamech",
    "Jubal"     , "Lamech",
    "Tubalcain" , "Lamech",
    "Enos"      , "Seth",
}

while gets
    chomp
    begin
        print $_, " "
    end while $_ = father[$_]
    puts
end

children = {}
father.each { |k,v|
    (children[v] ||= []) << k
}
while gets
    chomp
    puts "#{$_} begat #{(children[$_] || ['Nobody']).join(', ')}.\n"
end

includes = {}
files.each { |f|
    begin
        for l in IO.readlines(f)
            next unless l =~ /^\s*#\s*include\s*<([^>]+)>/
            (includes[$1] ||= []) << f
        end
    rescue SystemCallError
        $stderr.puts "#$! (skipping)"
    end
}

include_free = includes.values.flatten.uniq - includes.keys


# @@PLEAC@@_5.16
# dutree - print sorted intented rendition of du output
#% dutree
#% dutree /usr
#% dutree -a
#% dutree -a /bin

# The DuNode class collects all information about a directory,
# and provides some convenience methods
class DuNode

    attr_reader :name
    attr_accessor :size
    attr_accessor :kids

    def initialize(name)
        @name = name
        @kids = []
        @size = 0
    end

    # support for sorting nodes with side
    def size_compare(node2)
        @size <=> node2.size
    end

    def basename
        @name.sub(/.*\//, "")
    end

    #returns substring before last "/", nil if not there
    def parent
        p = @name.sub(/\/[^\/]+$/,"")
        if p == @name
            nil
        else
            p
        end
    end

end

# The DuTree does the acdtual work of
# getting the input, parsing it, builging up a tree
# and format it for output
class Dutree

    attr_reader :topdir

    def initialize
        @nodes = Hash.new
        @dirsizes = Hash.new(0)
        @kids = Hash.new([])
    end

    # get a node by name, create it if it does not exist yet
    def get_create_node(name)
        if @nodes.has_key?(name)
            @nodes[name]
        else
            node = DuNode.new(name)
            @nodes[name] = node
            node
        end
    end

    # run du, read in input, save sizes and kids
    # stores last directory read in instance variable topdir
    def input(arguments)
        name = ""
        cmd = "du " + arguments.join(" ")
        IO.popen(cmd) { |pipe|
            pipe.each { |line|
                size, name = line.chomp.split(/\s+/, 2)
                node = get_create_node(name)
                node.size = size.to_i
                @nodes[name] = node
                parent = node.parent
                if parent
                    get_create_node(parent).kids.push(node)
                end
            }
        }
        @topdir = @nodes[name]
    end

    # figure out how much is taken in each directory
    # that isn't stored in the subdirectories. Add a new
    # fake kid called "." containing that much.
    def get_dots(node)
        cursize = node.size
        for kid in node.kids
            cursize -=  kid.size
            get_dots(kid)
        end
        if node.size != cursize
            newnode = get_create_node(node.name + "/.")
            newnode.size = cursize
            node.kids.push(newnode)
        end
    end

    # recursively output everything
    # passing padding and number width as well
    # on recursive calls
    def output(node, prefix="", width=0)
        line = sprintf("%#{width}d %s", node.size, node.basename)
        puts(prefix + line)
        prefix += line.sub(/\d /, "| ")
        prefix.gsub!(/[^|]/, " ")
        if node.kids.length > 0     # not a bachelor node
            kids = node.kids
            kids.sort! { |a,b|
                b.size_compare(a)
            }
            width = kids[0].size.to_s.length
            for kid in kids
                output(kid, prefix, width)
            end
        end
    end

end

tree = Dutree.new
tree.input(ARGV)
tree.get_dots(tree.topdir)
tree.output(tree.topdir)


# @@PLEAC@@_6.0
# The verbose version are match, sub, gsub, sub! and gsub!;
# pattern needs to be a Regexp object; it yields a MatchData
# object.
pattern.match(string)
string.sub(pattern, replacement)
string.gsub(pattern, replacement)
# As usual in Ruby, sub! does the same as sub but also modifies
# the object, the same for gsub!/gsub.

# Sugared syntax yields the position of the match (or nil if no
# match). Note that the object at the right of the operator needs
# not to be a Regexp object (it can be a String). The "dont
# match" operator yields true or false.
meadow =~ /sheep/   # position of the match, nil if no match
meadow !~ /sheep/   # true if doesn't match, false if it does
# There is no sugared version for the substitution

meadow =~ /\bovines?\b/i and print "Here be sheep!"

string = "good food"
string.sub!(/o*/, 'e')

# % echo ababacaca | ruby -ne 'puts $& if /(a|ba|b)+(a|ac)+/'
# ababa

# The "global" (or "multiple") match is handled by String#scan
scan (/(\d+)/) {
    puts "Found number #{$1}"
}

# String#scan yields an Array if not used with a block
numbers = scan(/\d+/)

digits = "123456789"
nonlap = digits.scan(/(\d\d\d)/)
yeslap = digits.scan(/(?=(\d\d\d))/)
puts "Non-overlapping:  #{nonlap.join(' ')}"
puts "Overlapping:      #{yeslap.join(' ')}";
# Non-overlapping:  123 456 789
# Overlapping:      123 234 345 456 567 678 789

string = "And little lambs eat ivy"
string =~ /l[^s]*s/
puts "(#$`) (#$&) (#$')"
# (And ) (little lambs) ( eat ivy)


# @@PLEAC@@_6.1
# Ruby doesn't have the same problem:
dst = src.sub('this', 'that')

progname = $0.sub('^.*/', '')

bindirs = %w(/usr/bin /bin /usr/local/bin)
libdirs = bindirs.map { |l| l.sub('bin', 'lib') }


# @@PLEAC@@_6.3
/\S+/               # as many non-whitespace bytes as possible
/[A-Za-z'-]+/       # as many letters, apostrophes, and hyphens

/\b([A-Za-z]+)\b/   # usually best
/\s([A-Za-z]+)\s/   # fails at ends or w/ punctuation


# @@PLEAC@@_6.4
require 'socket'
str = 'www.ruby-lang.org and www.rubygarden.org'
re = /
      (               # capture the hostname in $1
        (?:           # these parens for grouping only
          (?! [-_] )  # lookahead for neither underscore nor dash
          [\w-] +     # hostname component
          \.          # and the domain dot
        ) +           # now repeat that whole thing a bunch of times
        [A-Za-z]      # next must be a letter
        [\w-] +       # now trailing domain part
      )               # end of $1 capture
     /x               # /x for nice formatting

str.gsub! re do       # pass a block to execute replacement
    host = TCPsocket.gethostbyname($1)
    "#{$1} [#{host[3]}]"
end

puts str
#-----------------------------
# to match whitespace or #-characters in an extended re you need to escape
# them.

foo = 42
str = 'blah #foo# blah'
str.gsub! %r/       # replace
              \#    #   a pound sign
              (\w+) #   the variable name
              \#    #   another pound sign
          /x do
              eval $1           # with the value of a local variable
          end
puts str  # => blah 42 blah


# @@PLEAC@@_6.5
# The 'g' modifier doesn't exist in Ruby, a regexp can't be used
# directly in a while loop; instead, use String#scan { |match| .. } 
fish = 'One fish two fish red fish blue fish'
WANT = 3
count = 0
fish.scan(/(\w+)\s+fish\b/i) {
    if (count += 1) == WANT
        puts "The third fish is a #{$1} one."
    end
}

if fish =~ /(?:\w+\s+fish\s+){2}(\w+)\s+fish/i
    puts "The third fish is a #{$1} one."
end

pond = 'One fish two fish red fish blue fish'
# String#scan without a block gives an array of matches, each match
# being an array of all the specified groups
colors = pond.scan(/(\w+)\s+fish\b/i).flatten  # get all matches
color  = colors[2]                          # then the one we want
# or without a temporary array
color = pond.scan(/(\w+)\s+fish\b/i).flatten[2]  # just grab element 3
puts "The third fish in the pond is #{color}."

count = 0
fishes = 'One fish two fish red fish blue fish'
evens = fishes.scan(/(\w+)\s+fish\b/i).select { (count+=1) % 2 == 0 }
print "Even numbered fish are #{evens.join(' ')}."

count = 0
fishes.gsub(/
   \b               # makes next \w more efficient
   ( \w+ )          # this is what we\'ll be changing
   (
     \s+ fish \b
   )
            /x) {
    if (count += 1) == 4
        'sushi' + $2
    else
        $1 + $2
    end
}

pond = 'One fish two fish red fish blue fish swim here.'
puts "Last fish is #{pond.scan(/\b(\w+)\s+fish\b/i).flatten[-1]}"

/
    A               # find some pattern A
    (?!             # mustn\'t be able to find
        .*          # something
        A           # and A
    )
    $               # through the end of the string
/x

# The "s" perl modifier is "m" in Ruby (not very nice since there is
# also an "m" in perl..)
pond = "One fish two fish red fish blue fish swim here."
if (pond =~ /
                    \b  (  \w+) \s+ fish \b
                (?! .* \b fish \b )
            /mix)
    puts "Last fish is #{$1}."
else
    puts "Failed!"
end


# @@PLEAC@@_6.6
#-----------------------------
#!/usr/bin/ruby -w
# killtags - very bad html killer
$/ = nil;                              # each read is whole file
while file = gets() do
    file.gsub!(/<.*?>/m,'');           # strip tags (terribly)
    puts file                          # print file to STDOUT
end
#-----------------------------
#!/usr/bin/ruby -w
#headerfy - change certain chapter headers to html
$/ = ''
while file = gets() do
    pattern = /
                  \A                   # start of record
                  (                    # capture in $1
                      Chapter          # text string
                      \s+              # mandatory whitespace
                      \d+              # decimal number
                      \s*              # optional whitespace
                      :                # a real colon
                      . *              # anything not a newline till end of line
                  )
               /x
    puts file.gsub(pattern,'<H1>\1</H1>')
end
#-----------------------------
#% ruby -00pe "gsub!(/\A(Chapter\s+\d+\s*:.*)/,'<H1>\1</H1>')" datafile

#!/usr/bin/ruby -w
#-----------------------------
for file in ARGV
    file = File.open(ARGV.shift)
    while file.gets('') do             # each read is a paragraph
        print "chunk #{$.} in $ARGV has <<#{$1}>>\n" while /^START(.*?)^END/m
    end                                # /m activates the multiline mode
end
#-----------------------------

# @@PLEAC@@_6.7
#-----------------------------
$/ = nil;
file = File.open("datafile")
chunks = file.gets.split(/pattern/)
#-----------------------------
# .Ch, .Se and .Ss divide chunks of STDIN
chunks = gets(nil).split(/^\.(Ch|Se|Ss)$/)
print "I read #{chunks.size} chunks.\n"
#-----------------------------


# @@PLEAC@@_6.8
while gets
    if ~/BEGIN/ .. ~/END/
        # line falls between BEGIN and END inclusive
    end
end

while gets
    if ($. == firstnum) .. ($. == lastnum)
        # operate between firstnum and lastnum line number
    end
end

# in ruby versions prior to 1.8, the above two conditional
# expressions could be shortened to:
#     if /BEGIN/ .. /END/
# and
#     if firstnum .. lastnum
# but these now only work this way from the command line  

#-----------------------------

while gets
    if ~/BEGIN/ ... ~/END/
        # line falls between BEGIN and END on different lines
    end
end

while gets
    if ($. == first) ... ($. == last)
        # operate between first and last line number on different lines
    end
end

#-----------------------------
# command-line to print lines 15 through 17 inclusive (see below)
ruby -ne 'print if 15 .. 17' datafile

# print out all <XMP> .. </XMP> displays from HTML doc
while gets
    print if ~%r#<XMP>#i .. ~%r#</XMP>#i;
end
    
# same, but as shell command
# ruby -ne 'print if %r#<XMP>#i .. %r#</XMP>#i' document.html
#-----------------------------
# ruby -ne 'BEGIN { $top=3; $bottom=5 }; \
#     print if $top .. $bottom' /etc/passwd                 #  FAILS
# ruby -ne 'BEGIN { $top=3; $bottom=5 }; \
#     print if $. == $top .. $. ==  $bottom' /etc/passwd    # works
# ruby -ne 'print if 3 .. 5' /etc/passwd                    # also works
#-----------------------------
print if ~/begin/ .. ~/end/;
print if ~/begin/ ... ~/end/;
#-----------------------------
while gets
    $in_header = $. == 1  .. ~/^$/ ? true : false
    $in_body   = ~/^$/ .. ARGF.eof ? true : false
end
#-----------------------------
seen = {}
ARGF.each do |line|
    next unless line =~ /^From:?\s/i .. line =~ /^$/;
    line.scan(%r/([^<>(),;\s]+\@[^<>(),;\s]+)/).each do |addr|
        puts addr unless seen[addr]
        seen[addr] ||= 1 
    end
end


# @@PLEAC@@_6.9
def glob2pat(globstr)
    patmap = {
        '*' => '.*',
        '?' => '.',
        '[' => '[',
        ']' => ']',
    }
    globstr.gsub!(/(.)/) { |c| patmap[c] || Regexp::escape(c) }
    '^' + globstr + '$'
end


# @@PLEAC@@_6.10
# avoid interpolating patterns like this if the pattern
# isn't going to change:
pattern = ARGV.shift
ARGF.each do |line|
    print line if line =~ /#{pattern}/
end

# the above creates a new regex each iteration. Instead,
# use the /o modifier so the regex is compiled only once

pattern = ARGV.shift
ARGF.each do |line|
    print line if line =~ /#{pattern}/o
end

#-----------------------------

#!/usr/bin/ruby
# popgrep1 - grep for abbreviations of places that say "pop"
# version 1: slow but obvious way
popstates = %w(CO ON MI WI MN)
ARGF.each do |line|
    popstates.each do |state|
        if line =~ /\b#{state}\b/
            print line
            last
        end
    end
end

#-----------------------------
#!/usr/bin/ruby
# popgrep2 - grep for abbreviations of places that say "pop"
# version 2: eval strings; fast but hard to quote
popstates = %w(CO ON MI WI MN)
code = "ARGF.each do |line|\n"
popstates.each do |state|
    code += "\tif line =~ /\\b#{state}\\b/; print(line); next; end\n"
end
code += "end\n"
print "CODE IS\n---\n#{code}\n---\n" if false # turn on for debugging
eval code

# CODE IS
# ---
# ARGF.each do |line|
#         if line =~ /\bCO\b/; print(line); next; end
#         if line =~ /\bON\b/; print(line); next; end
#         if line =~ /\bMI\b/; print(line); next; end
#         if line =~ /\bWI\b/; print(line); next; end
#         if line =~ /\bMN\b/; print(line); next; end
# end
# 
# ---

## alternatively, the same idea as above but compiling 
## to a case statement: (not in perlcookbook)
#!/usr/bin/ruby -w
# popgrep2.5 - grep for abbreviations of places that say "pop"
# version 2.5: eval strings; fast but hard to quote
popstates = %w(CO ON MI WI MN)
code = "ARGF.each do |line|\n    case line\n"
popstates.each do |state|
    code += "        when /\\b#{state}\\b/ : print line\n"
end
code += "    end\nend\n"
print "CODE IS\n---\n#{code}\n---\n" if false # turn on for debugging
eval code

# CODE IS
# ---
# ARGF.each do |line|
#     case line
#         when /\bCO\b/ : print line
#         when /\bON\b/ : print line
#         when /\bMI\b/ : print line
#         when /\bWI\b/ : print line
#         when /\bMN\b/ : print line
#     end
# end
# 
# ---

# Note: (above) Ruby 1.8+ allows the 'when EXP : EXPR' on one line
# with the colon separator.

#-----------------------------
#!/usr/bin/ruby
# popgrep3 - grep for abbreviations of places that say "pop"
# version3: build a match_any function
popstates = %w(CO ON MI WI MN)
expr = popstates.map{|e|"line =~ /\\b#{e}\\b/"}.join('||')
eval "def match_any(line); #{expr};end"
ARGF.each do |line|
    print line if match_any(line)
end
#-----------------------------

##  building a match_all function is a trivial
##  substitution of && for ||
##  here is a generalized example:
#!/usr/bin/ruby -w
## grepauth - print lines that mention both foo and bar
class MultiMatch 
    def initialize(*patterns)
        _any = build_match('||',patterns)
        _all = build_match('&&',patterns)
        eval "def match_any(line);#{_any};end\n"
        eval "def match_all(line);#{_all};end\n"
      end
    def build_match(sym,args)
        args.map{|e|"line =~ /#{e}/"}.join(sym)
    end
end

mm = MultiMatch.new('foo','bar')
ARGF.each do |line|
    print line if mm.match_all(line)
end
#-----------------------------

#!/usr/bin/ruby
# popgrep4 - grep for abbreviations of places that say "pop"
# version4: pretty fast, but simple: compile all re's first:
popstates = %w(CO ON MI WI MN)
popstates = popstates.map{|re| %r/\b#{re}\b/}
ARGF.each do |line|
    popstates.each do |state_re|
        if line =~ state_re
            print line
            break
        end
    end
end

## speeds trials on the jargon file(412): 26006 lines, 1.3MB
## popgrep1   => 7.040s
## popgrep2   => 0.656s
## popgrep2.5 => 0.633s
## popgrep3   => 0.675s
## popgrep4   => 1.027s

# unless speed is criticial, the technique in popgrep4 is a
# reasonable balance between speed and logical simplicity.


# @@PLEAC@@_6.11
begin
    print "Pattern? "
    pat = $stdin.gets.chomp
    Regexp.new(pat)
rescue 
    warn "Invalid Pattern"
    retry
end


# @@PLEAC@@_6.13
# uses the 'amatch' extension found on:
# http://raa.ruby-lang.org/project/amatch/
require 'amatch'
matcher = Amatch.new('balast')
#$relative, $distance = 0, 1
File.open('/usr/share/dict/words').each_line do |line|
    print line if matcher.search(line) <= 1
end
__END__
#CODE
ballast
ballasts
balustrade
balustrades
blast
blasted
blaster
blasters
blasting
blasts


# @@PLEAC@@_6.14
str.scan(/\G(\d)/).each do |token|
    puts "found #{token}"
end
#-----------------------------
n = "   49 here"
n.gsub!(/\G /,'0')
puts n
#-----------------------------
str = "3,4,5,9,120"
str.scan(/\G,?(\d+)/).each do |num|
    puts "Found number: #{num}"
end
#-----------------------------
# Ruby doesn't have the String.pos or a /c re modifier like Perl 
# But it does have StringScanner in the standard library (strscn)
# which allows similar functionality:

require 'strscan'
text = 'the year 1752 lost 10 days on the 3rd of September'
sc = StringScanner.new(text)
while sc.scan(/.*?(\d+)/)
    print "found: #{sc[1]}\n"   
end
if sc.scan(/\S+/)
    puts "Found #{sc[0]} after last number"
end
#-----------------------------
# assuming continuing from above:
puts "The position in 'text' is: #{sc.pos}"
sc.pos = 30
puts "The position in 'text' is: #{sc.pos}"


# @@PLEAC@@_6.15
#-----------------------------
# greedy pattern
str.gsub!(/<.*>/m,'')   # not good

# non-greedy (minimal) pattern
str.gsub!(/<.*?>/m,'')   # not great


#-----------------------------
#<b><i>this</i> and <i>that</i> are important</b> Oh, <b><i>me too!</i></b>
#-----------------------------
%r{ <b><i>(.*?)</i></b> }mx
#-----------------------------
%r/BEGIN((?:(?!BEGIN).)*)END/
#-----------------------------
%r{ <b><i>(  (?: (?!</b>|</i>). )*  ) </i></b> }mx
#-----------------------------
%r{ <b><i>(  (?: (?!</[ib]>). )*  ) </i></b> }mx
#-----------------------------
%r{
    <b><i> 
    [^<]*  # stuff not possibly bad, and not possibly the end.
    (?:
 # at this point, we can have '<' if not part of something bad
     (?!  </?[ib]>  )   # what we can't have
     <                  # okay, so match the '<'
     [^<]*              # and continue with more safe stuff
    ) *
    </i></b>
 }mx


# @@PLEAC@@_6.16
#-----------------------------
$/ = ""
ARGF.each do |para|
    para.scan %r/
                  \b     # start at word boundary
                  (\S+)  # find chunk of non-whitespace
                  \b     # until a word boundary
                  (      
                    \s+  # followed by whitespace
                    \1   # and that same chunk again
                    \b   # and a word boundary
                  ) +    # one or more times
                /xi do
        puts "dup word '#{$1}' at paragraph #{$.}" 
    end
end
#-----------------------------
astr = 'nobody'
bstr = 'bodysnatcher'
if "#{astr} #{bstr}" =~ /^(\w+)(\w+) \2(\w+)$/
    print "#{$2} overlaps in #{$1}-#{$2}-#{$3}"
end
#-----------------------------
#!/usr/bin/ruby -w
# prime_pattern -- find prime factors of argument using patterns
ARGV << 180
cap = 'o' * ARGV.shift
while cap =~ /^(oo+?)\1+$/
    print $1.size, " "
    cap.gsub!(/#{$1}/,'o')
end
puts cap.size
#-----------------------------
#diophantine
# solve for 12x + 15y + 16z = 281, maximizing x
if ('o' * 281).match(/^(o*)\1{11}(o*)\2{14}(o*)\3{15}$/)
    x, y, z = $1.size, $2.size, $3.size
    puts "One solution is: x=#{x}; y=#{y}; z=#{z}"
else 
    puts "No solution."
end
#    => One solution is: x=17; y=3; z=2

#-----------------------------
# using different quantifiers:
('o' * 281).match(/^(o+)\1{11}(o+)\2{14}(o+)\3{15}$/)
#    => One solution is: x=17; y=3; z=2

('o' * 281).match(/^(o*?)\1{11}(o*)\2{14}(o*)\3{15}$/)
#    => One solution is: x=0; y=7; z=11

('o' * 281).match(/^(o+?)\1{11}(o*)\2{14}(o*)\3{15}$/)
#    => One solution is: x=1; y=3; z=14


# @@PLEAC@@_6.17
# alpha OR beta
%r/alpha|beta/

# alpha AND beta
%r/(?=.*alpha)(?=.*beta)/m

# alpha AND beta,  no overlap
%r/alpha.*beta|beta.*alpha/m

# NOT beta
%r/^(?:(?!beta).)*$/m

# NOT bad BUT good
%r/(?=(?:(?!BAD).)*$)GOOD/m
#-----------------------------

if !(string =~ /pattern/)   # ugly
    something()
end

if string !~ /pattern/   # preferred
    something()
end


#-----------------------------
if string =~ /pat1/  && string =~ /pat2/
    something()
end
#-----------------------------
if string =~ /pat1/ || string =~ /pat2/
    something()
end
#-----------------------------
#!/usr/bin/ruby -w
# minigrep - trivial grep
pat = ARGV.shift
ARGF.each do |line|
    print line if line =~ /#{pat}/o
end
#-----------------------------
 "labelled" =~ /^(?=.*bell)(?=.*lab)/m
#-----------------------------
$string =~ /bell/ && $string =~ /lab/
#-----------------------------
$murray_hill = "blah bell blah "
if $murray_hill =~ %r{
                         ^              # start of string
                        (?=             # zero-width lookahead
                            .*          # any amount of intervening stuff
                            bell        # the desired bell string
                        )               # rewind, since we were only looking
                        (?=             # and do the same thing
                            .*          # any amount of intervening stuff
                            lab         # and the lab part
                        )
                     }mx                # /m means . can match newline

    print "Looks like Bell Labs might be in Murray Hill!\n";
end
#-----------------------------
"labelled" =~ /(?:^.*bell.*lab)|(?:^.*lab.*bell)/
#-----------------------------
$brand = "labelled";
if $brand =~ %r{
                (?:                 # non-capturing grouper
                    ^ .*?           # any amount of stuff at the front
                      bell          # look for a bell
                      .*?           # followed by any amount of anything
                      lab           # look for a lab
                  )                 # end grouper
            |                       # otherwise, try the other direction
                (?:                 # non-capturing grouper
                    ^ .*?           # any amount of stuff at the front
                      lab           # look for a lab
                      .*?           # followed by any amount of anything
                      bell          # followed by a bell
                  )                 # end grouper
            }mx                     # /m means . can match newline
    print "Our brand has bell and lab separate.\n";
end
#-----------------------------
$map =~ /^(?:(?!waldo).)*$/s
#-----------------------------
$map = "the great baldo"
if $map =~ %r{
                ^                   # start of string
                (?:                 # non-capturing grouper
                    (?!             # look ahead negation
                        waldo       # is he ahead of us now?
                    )               # is so, the negation failed
                    .               # any character (cuzza /s)
                ) *                 # repeat that grouping 0 or more
                $                   # through the end of the string
             }mx                    # /m means . can match newline
    print "There's no waldo here!\n";
end
=begin
 7:15am  up 206 days, 13:30,  4 users,  load average: 1.04, 1.07, 1.04

USER     TTY      FROM              LOGIN@  IDLE   JCPU   PCPU  WHAT

tchrist  tty1                       5:16pm 36days 24:43   0.03s  xinit

tchrist  tty2                       5:19pm  6days  0.43s  0.43s  -tcsh

tchrist  ttyp0    chthon            7:58am  3days 23.44s  0.44s  -tcsh

gnat     ttyS4    coprolith         2:01pm 13:36m  0.30s  0.30s  -tcsh
=end
#% w | minigrep '^(?!.*ttyp).*tchrist'
#-----------------------------
%r{
    ^                       # anchored to the start
    (?!                     # zero-width look-ahead assertion
        .*                  # any amount of anything (faster than .*?)
        ttyp                # the string you don't want to find
    )                       # end look-ahead negation; rewind to start
    .*                      # any amount of anything (faster than .*?)
    tchrist                 # now try to find Tom
}x
#-----------------------------
#% w | grep tchrist | grep -v ttyp
#-----------------------------
#% grep -i 'pattern' files
#% minigrep '(?i)pattern' files
#-----------------------------


# @@PLEAC@@_6.20
ans = $stdin.gets.chomp
re = %r/^#{Regexp.quote(ans)}/
case 
    when "SEND"  =~ re : puts "Action is send"
    when "STOP"  =~ re : puts "Action is stop"
    when "ABORT" =~ re : puts "Action is abort"
    when "EDIT"  =~ re : puts "Action is edit"
end
#-----------------------------
require 'abbrev'
table = Abbrev.abbrev %w-send stop abort edit-
loop do
    print "Action: "
    ans = $stdin.gets.chomp
    puts "Action for #{ans} is #{table[ans.downcase]}"
end


#-----------------------------
# dummy values are defined for 'file', 'PAGER', and
# the 'invoke_editor' and 'deliver_message' methods
# do not do anything interesting in this example.
#!/usr/bin/ruby -w
require 'abbrev'

file = 'pleac_ruby.data'
PAGER = 'less'

def invoke_editor
    puts "invoking editor"
end

def deliver_message
    puts "delivering message"
end

actions = {
    'edit'  => self.method(:invoke_editor),
    'send'  => self.method(:deliver_message),
    'list'  => proc {system(PAGER, file)},
    'abort' => proc {puts "See ya!"; exit},
    ""      => proc {puts "Unknown Command"}
}

dtable = Abbrev.abbrev(actions.keys)
loop do
    print "Action: "
    ans = $stdin.gets.chomp.delete(" \t")
    actions[ dtable[ans.downcase] || "" ].call
end


# @@PLEAC@@_6.19
#-----------------------------
# basically, the Perl Cookbook categorizes this as an
# unsolvable problem ...
#-----------------------------
1 while addr.gsub!(/\([^()]*\)/,'')
#-----------------------------
Dear someuser@host.com,

Please confirm the mail address you gave us Wed May  6 09:38:41
MDT 1998 by replying to this message.  Include the string
"Rumpelstiltskin" in that reply, but spelled in reverse; that is,
start with "Nik...".  Once this is done, your confirmed address will
be entered into our records.


# @@PLEAC@@_6.21
#-----------------------------
#% gunzip -c ~/mail/archive.gz | urlify > archive.urlified
#-----------------------------
#% urlify ~/mail/*.inbox > ~/allmail.urlified
#-----------------------------
#!/usr/bin/ruby -w
# urlify - wrap HTML links around URL-like constructs

urls = '(https?|telnet|gopher|file|wais|ftp)';
ltrs = '\w';
gunk = '/#~:.?+=&%@!\-';
punc = '.:?\-';
any  = "#{ltrs}#{gunk}#{punc}";

ARGF.each do |line|
    line.gsub! %r/
        \b                    # start at word boundary
        (                     # begin $1  {
         #{urls}     :        # need resource and a colon
         [#{any}] +?          # followed by on or more
                              #  of any valid character, but
                              #  be conservative and take only
                              #  what you need to....
        )                     # end   $1  }
        (?=                   # look-ahead non-consumptive assertion
         [#{punc}]*           # either 0 or more punctuation
         [^#{any}]            #   followed by a non-url char
         |                    # or else
         $                    #   then end of the string
        )
    /iox do 
        %Q|<A HREF="#{$1}">#{$1}</A>|
    end
    print line
end


# @@PLEAC@@_6.23
%r/^m*(d?c{0,3}|c[dm])(l?x{0,3}|x[lc])(v?i{0,3}|i[vx])$/i
#-----------------------------
str.sub!(/(\S+)(\s+)(\S+)/, '\3\2\1')
#-----------------------------
%r/(\w+)\s*=\s*(.*)\s*$/             # keyword is $1, value is $2
#-----------------------------
%r/.{80,}/
#-----------------------------
%r|(\d+)/(\d+)/(\d+) (\d+):(\d+):(\d+)|
#-----------------------------
str.gsub!(%r|/usr/bin|,'/usr/local/bin')
#-----------------------------
str.gsub!(/%([0-9A-Fa-f][0-9A-Fa-f])/){ $1.hex.chr }
#-----------------------------
str.gsub!(%r{
    /\*                    # Match the opening delimiter
    .*?                    # Match a minimal number of characters
    \*/                    # Match the closing delimiter
}xm,'')
#-----------------------------
str.sub!(/^\s+/, '')
str.sub!(/\s+$/, '')

# but really, in Ruby we'd just do:
str.strip!
#-----------------------------
str.gsub!(/\\n/,"\n")
#-----------------------------
str.sub!(/^.*::/, '')
#-----------------------------
%r/^([01]?\d\d|2[0-4]\d|25[0-5])\.([01]?\d\d|2[0-4]\d|25[0-5])\.
    ([01]?\d\d|2[0-4]\d|25[0-5])\.([01]?\d\d|2[0-4]\d|25[0-5])$/x
#-----------------------------
str.sub!(%r|^.*/|, '')
#-----------------------------
cols = ( (ENV['TERMCAP'] || " ") =~ /:co#(\d+):/ ) ? $1 : 80;
#-----------------------------
name = " #{$0} #{ARGV}".gsub(%r| /\S+/|, ' ')
#-----------------------------
require 'rbconfig'
include Config
raise "This isn't Linux" unless CONFIG['target_os'] =~ /linux/i;
#-----------------------------
str.gsub!(%r/\n\s+/, ' ')
#-----------------------------
nums = str.scan(/(\d+\.?\d*|\.\d+)/)
#-----------------------------
capwords = str.scan(%r/(\b[^\Wa-z0-9_]+\b)/)
#-----------------------------
lowords = str.scan(%r/(\b[^\WA-Z0-9_]+\b)/)
#-----------------------------
icwords = str.scan(%r/(\b[^\Wa-z0-9_][^\WA-Z0-9_]*\b)/)
#-----------------------------
links = str.scan(%r/<A[^>]+?HREF\s*=\s*["']?([^'" >]+?)[ '"]?>/mi)
#-----------------------------
initial = str =~ /^\S+\s+(\S)\S*\s+\S/ ? $1 : ""
#-----------------------------
str.gsub!(%r/"([^"]*)"/, %q-``\1''-)
#-----------------------------

$/ = ""
sentences = []
ARGF.each do |para|
    para.gsub!(/\n/, ' ')
    para.gsub!(/ {3,}/,'  ')
    sentences << para.scan(/(\S.*?[!?.])(?=  |\Z)/)
end

#-----------------------------
%r/(\d{4})-(\d\d)-(\d\d)/            # YYYY in $1, MM in $2, DD in $3
#-----------------------------
%r/ ^
      (?:
       1 \s (?: \d\d\d \s)?            # 1, or 1 and area code
       |                               # ... or ...
       \(\d\d\d\) \s                   # area code with parens
       |                               # ... or ...
       (?: \+\d\d?\d? \s)?             # optional +country code
       \d\d\d ([\s\-])                 # and area code
      )
      \d\d\d (\s|\1)                   # prefix (and area code separator)
      \d\d\d\d                         # exchange
        $
 /x
#-----------------------------
%r/\boh\s+my\s+gh?o(d(dess(es)?|s?)|odness|sh)\b/i
#-----------------------------
lines = []
lines << $1 while input.sub!(/^([^\012\015]*)(\012\015?|\015\012?)/,'')


# @@PLEAC@@_7.0
# An IO object being Enumerable, we can use 'each' directly on it
File.open("/usr/local/widgets/data").each { |line|
    puts line if line =~ /blue/
}

logfile = File.new("/var/log/rubylog.txt", "w")
mysub($stdin, logfile)

# The method IO#readline is similar  to IO#gets
# but throws an exception when it reaches EOF
f = File.new("bla.txt")
begin
    while (line = f.readline)
        line.chomp
        $stdout.print line if line =~ /blue/
    end
rescue EOFError
    f.close
end

while $stdin.gets                        # reads from STDIN
    unless (/\d/) 
        $stderr.puts "No digit found."   # writes to STDERR
    end
    puts "Read: #{$_}"                   # writes to STDOUT
end

logfile = File.new("/tmp/log", "w")

logfile.close

# $defout (or its synonym '$>') is the destination of output
# for Kernel#print, Kernel#puts, and family functions
logfile = File.new("log.txt", "w")
old = $defout
$defout = logfile                 # switch to logfile for output
puts "Countdown initiated ..."
$defout = old                     # return to original output
puts "You have 30 seconds to reach minimum safety distance."


# @@PLEAC@@_7.1
source = File.new(path, "r")  # open file "path" for reading only
sink   = File.new(path, "w")  # open file "path" for writing only

source = File.open(path, File::RDONLY)  # open file "path" for reading only
sink   = File.open(path, File::WRONLY)  # open file "path" for writing only

file   = File.open(path, "r+")  # open "path" for reading and writing
file   = File.open(path, flags) # open "path" with the flags "flags" (see examples below for flags)

# open file "path" read only
file   = File.open(path, "r")
file   = File.open(path, File::RDONLY)

# open file "path" write only, create it if it does not exist
# truncate it to zero length if it exists
file   = File.open(path, "w")
file   = File.open(path, File::WRONLY|File::TRUNC|File::CREAT) 
file   = File.open(path, File::WRONLY|File::TRUNC|File::CREAT, 0666)  # with permission 0666

# open file "path" write only, fails if file exists
file   = File.open(path, File::WRONLY|File::EXCL|File::CREAT) 
file   = File.open(path, File::WRONLY|File::EXCL|File::CREAT, 0666) 

# open file "path" for appending
file   = File.open(path, "a")
file   = File.open(path, File::WRONLY|File::APPEND|File::CREAT) 
file   = File.open(path, File::WRONLY|File::APPEND|File::CREAT, 0666) 

# open file "path" for appending only when file exists
file   = File.open(path, File::WRONLY|File::APPEND) 

# open file "path" for reading and writing
file   = File.open(path, "r+")
file   = File.open(path, File::RDWR)

# open file for reading and writing, create a new file if it does not exist
file   = File.open(path, File::RDWR|File::CREAT)
file   = File.open(path, File::RDWR|File::CREAT, 0600)

# open file "path" reading and writing, fails if file exists
file   = File.open(path, File::RDWR|File::EXCL|File::CREAT)
file   = File.open(path, File::RDWR|File::EXCL|File::CREAT, 0600)


# @@PLEAC@@_7.2
# No problem with Ruby since the filename doesn't contain characters with
# special meaning; like Perl's sysopen
File.open(filename, 'r')


# @@PLEAC@@_7.3
File.expand_path('~root/tmp')
#=> "/root/tmp"
File.expand_path('~rpcuser')
#=> "/var/lib/nfs"

# To expand ~/.. it explicitely needs the environment variable HOME
File.expand_path('~/tmp')
#=> "/home/gc/tmp"


# @@PLEAC@@_7.4
# The exception raised in Ruby reports the filename
File.open('afile')


# @@PLEAC@@_7.5
# Standard Ruby distribution provides the following useful extension
require 'tempfile'
# With the Tempfile class, the file is automatically deleted on garbage
# collection, so you won't need to remove it, later on.
tf = Tempfile.new('tmp')   # a name is required to create the filename

# If you need to pass the filename to an external program you can use
# File#path, but don't forget to File#flush in order to flush anything
# living in some buffer somewhere.
tf.flush
system("/usr/bin/dowhatever #{tf.path}")

fh = Tempfile.new('tmp')
fh.sync = true                # autoflushes
10.times { |i| fh.puts i }
fh.rewind
puts 'Tmp file has: ', fh.readlines


# @@PLEAC@@_7.6
while (DATA.gets) do
    # process the line  
end
__END__
# your data goes here
# __DATA__ doesn't exist in Ruby

#CODE
# get info about the script (size, date of last modification)
kilosize = DATA.stat.size / 1024
last_modif = DATA.stat.mtime
puts "<P>Script size is #{kilosize}"
puts "<P>Last script update: #{last_modif}"
__END__
# DO NOT REMOVE THE PRECEEDING LINE.
# Everything else in this file will be ignored.
#CODE


# @@PLEAC@@_7.7
while line = gets do
    # do something with line.
end

#  or 
while gets do
    # do something with $_
end

# or more rubyish
$stdin.each do |line|
    # do stuff with line
end


# ARGF may makes this more easy
# this is skipped if ARGV.size==0
ARGV.each do |filename| 
    # closing and exception handling are done by the block
    open(filename) do |fd| 
        fd.each do |line|
            # do stuff with line
        end   
    end rescue abort("can't open %s" % filename)
end

# globbing is done in the Dir module
ARGV = Dir["*.[Cch]"] if ARGV.empty?

# note: optparse is the preferred way to handle this
if (ARGV[0] == '-c')  
    chop_first += 1
    ARGV.shift
end


# processing numerical options
if ARGV[0] =~ /^-(\d+)$/
    columns = $1
    ARGV.shift
end

# again, better to use optparse:
require 'optparse'
nostdout = 0
append = 0
unbuffer = 0
ignore_ints = 0
ARGV.options do |opt|
    opt.on('-n') { nostdout +=1 }
    opt.on('-a') { append   +=1 }
    opt.on('-u') { unbuffer +=1 }
    opt.on('-i') { ignore_ints +=1 }
    opt.parse!
end or abort("usage: " + __FILE__ + " [-ainu] [filenames]")

# no need to do undef $/, we have File.read
str = File.read(ARGV[0])

# again we have File.read
str = File.read(ARGV[0])

# not sure what this should do:
# I believe open the file, print filename, lineno and line:
ARGF.each_with_index do |line, idx|
    print ARGF.filename, ":", idx, ";", line
end

# print all the lines in every file passed via command line that contains login
ARGF.each do |line|
    puts line if line =~ /login/
end
#
# even this would fit
#%ruby -ne "print if /f/" 2.log
#

ARGF.each { |l| puts l.downcase! }

#------------------
#!/usr/bin/ruby -p
# just like perl's -p
$_.downcase!
#

# I don't know who should I trust. 
# perl's version splits on \w+ while python's on \w.

chunks = 0

File.read(ARGV[0]).split.each do |word|
    next if word =~ /^#/
    break if ["__DATA__", "__END__"].member? word
    chunks += 1 
end

print "Found ", chunks, " chunks\n"


# @@PLEAC@@_7.8
old = File.open(old_file)
new = File.open(new_file, "w")
while old.gets do
    # change $_, then...
    new.print $_
end
old.close
new.close
File.rename(old_file, "old.orig")
File.rename(new_file, old_file)

while old.gets do
    if $. == 20 then # we are at the 20th line
        new.puts "Extra line 1"
        new.puts "Extra line 2"
    end
    new.print $_
end

while old.gets do
    next if 20..30 # skip the 20th line to the 30th
                   # Ruby (and Perl) permit to write if 20..30 
                   # instead of if (20 <= $.) and ($. <= 30)
    new.print $_
end


# @@PLEAC@@_7.9
#% ruby -i.orig -pe 'FILTER COMMAND' file1 file2 file3 ...
#
#-----------------------------
##!/usr/bin/ruby -i.orig -p
# filter commands go here
#-----------------------------

#% ruby -pi.orig -e 'gsub!(/DATE/){Time.now)'

# effectively becomes:
ARGV << 'I'
oldfile = ""
while gets
    if ARGF.filename != oldfile
        newfile = ARGF.filename
        File.rename(newfile, newfile + ".orig")
        $stdout = File.open(newfile,'w')
        oldfile = newfile
    end
    gsub!(/DATE/){Time.now}
    print 
end
$stdout = STDOUT
#-----------------------------
#% ruby -i.old -pe 'gsub!(%r{\bhisvar\b}, 'hervar')' *.[Cchy]

#-----------------------------
# set up to iterate over the *.c files in the current directory,
# editing in place and saving the old file with a .orig extension
$-i = '.orig'                       # set up -i mode
ARGV.replace(Dir['*.[Cchy]'])
while gets
    if $. == 1
        print "This line should appear at the top of each file\n"
    end
    gsub!(/\b(p)earl\b/i, '\1erl')    # Correct typos, preserving case
    print
    ARGF.close if ARGF.eof
end


# @@PLEAC@@_7.10
File.open('itest', 'r+') do |f|   # open file for update
    lines = f.readlines           # read into array of lines
    lines.each do |it|            # modify lines
        it.gsub!(/foo/, 'QQQ')
    end
    f.pos = 0                     # back to start
    f.print lines                 # write out modified lines
    f.truncate(f.pos)             # truncate to new length
end                               # file is automatically closed
#-----------------------------
File.open('itest', 'r+') do |f|   
    out = ""
    f.each do |line|
        out << line.gsub(/DATE/) {Time.now}
    end
    f.pos = 0                     
    f.print out
    f.truncate(f.pos)             
end

# @@PLEAC@@_7.11
File.open('infile', 'r+') do |f|
    f.flock File::LOCK_EX
    # update file
end
#-----------------------------
File::LOCK_SH     # shared lock (for reading)
File::LOCK_EX     # exclusive lock (for writing)
File::LOCK_NB     # non-blocking request
File::LOCK_UN     # free lock
#-----------------------------
unless f.flock File::LOCK_EX | File::LOCK_NB
    warn "can't get immediate lock: blocking ..."
    f.flock File::LOCK_EX 
end
#-----------------------------
File.open('numfile', File::RDWR|File::CREAT) do |f|
    f.flock(File::LOCK_EX)
    num = f.gets.to_i || 0
    f.pos = 0
    f.truncate 0
    f.puts num + 1q
end


# @@PLEAC@@_7.12
output_handle.sync = true
# Please note that like in Perl, $stderr is already unbuffered
#-----------------------------
#!/usr/bin/ruby -w
# seeme - demo stdio output buffering
$stdout.sync = ARGV.size > 0
print "Now you don't see it..."
sleep 2
puts "now you do"
#-----------------------------
$stderr.sync = true
afile.sync = false
#-----------------------------
# assume 'remote_con' is an interactive socket handle,
# but 'disk_file' is a handle to a regular file.
remote_con.sync = true       # unbuffer for clarity
disk_file.sync = false       # buffered for speed
#-----------------------------
require 'socket'
sock = TCPSocket.new('www.ruby-lang.org', 80)
sock.sync = true
sock.puts "GET /en/ HTTP/1.0 \n\n"
resp = sock.read
print "DOC IS: #{resp}\n"


# @@PLEAC@@_7.13
#-----------------------------
# assumes fh1, fh2, fh2 are oen IO objects
nfound = select([$stdin, fh1, fh2, fh3], nil, nil, 0)
nfound[0].each do |file|
    case file
        when fh1
            # do something with fh1
        when fh2
            # do something with fh2
        when fh3
            # do something with fh3
    end
end
#-----------------------------
input_files = []
# repeat next line for all in-files to poll
input_files << fh1
if nfound = select(input_files, nil, nil, 0)
    # input ready on files in nfound[0]
end


# @@PLEAC@@_8.0
#-----------------------------
# datafile is a file or IO object
datafile.readlines.each { |line|
    line.chomp!
    size = line.length
    puts size
}
#-----------------------------
datafile.readlines.each { |line|
    puts line.chomp!.length
}
#-----------------------------
lines = datafile.readlines
#-----------------------------
whole_file = file.read
#-----------------------------
# ruby -040 -e 'word = gets; puts "First word is #{word}"'
#-----------------------------
# ruby -ne 'BEGIN { $/="%%\n" }; $_.chomp; puts $_ if( $_=~/Unix/i)' fortune.dat
#-----------------------------
handle.print "one", "two", "three" # "onetwothree"
puts "Baa baa black sheep."        # sent to $stdout
#-----------------------------
buffer = handle.read(4096)
rv     = buffer.length
#-----------------------------
handle.truncate(length)
open("/tmp#{$$}.pid", 'w') { |handle| handle.truncate(length) }
#-----------------------------
pos = datafile.pos  # tell is an alias of pos
puts "I'm #{pos} bytes from the start of datafile"
#-----------------------------
logfile.seek(0, IO::SEEK_END)
datafile.seek(pos)  #  IO::SEEK_SET is the default
out.seek(-20, IO::SEEK_CUR)
#-----------------------------
written = datafile.syswrite(mystring)
raise RunTimeError unless written == mystring.length
block = infile.sysread(256)   # no equivalent to perl offset parameter in sysread
puts "only read #{block.length} bytes" if 256 != block.length
#-----------------------------
pos = handle.sysseek(0, IO::SEEK_CUR)  # don't change position


# @@PLEAC@@_8.1
while (line = fh.gets)
    line.chomp!
    nextline = nil
    line.gsub!(/\\$/) { |match| nextline = fh.gets; '' }
    if (nextline != nil)
        line += nextline 
        redo
    end
    # process full record in line here
end
#-----------------------------
# DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) \
#         $(TEXINFOS) $(INFOS) $(MANS) $(DATA)
# DEP_DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) \
#         $(TEXINFOS) $(INFO_DEPS) $(MANS) $(DATA) \
#         $(EXTRA_DIST)
#-----------------------------
line.gsub!(/\\\s*$/, '') {
    # as before
}


# @@PLEAC@@_8.2
#-----------------------------
count = `wc -l < #{filename}`
fail "wc failed: #{$?}" if $? != 0
count.chomp!
#-----------------------------
count = 0
File.open(file, 'r') { |fh|
    count += 1 while fh.gets
}
# count now holds the number of lines read
#-----------------------------
count = 0
while (chunk = file.sysread(2**16)) 
    count += chunk.count("\n")
end rescue EOFError
#-----------------------------
File.open(filename,'r') { |fh|
    count += 1 while fh.gets
}
# count now holds the number of lines read
#-----------------------------
# As ruby doesn't quite have an equivalent to using a for
# statement as in perl, I threw this in
count = File.readlines(filename).size
#-----------------------------
1 while file.gets
count = $.
#-----------------------------
$/ = ''
open(filename, 'r') { |fh|
    1 while fh.gets
    para_count = $.
} rescue fail("can't open #{filename}: $!") 
#-----------------------------


# ^^PLEAC^^_8.3
#-----------------------------
while (gets)
    split.each { |chunk|
        # do something with chunk
    }
end
#-----------------------------
while (gets)
    gsub(/(\w[\w'-]*)/) { |word|
        # do something with word
    }
end
#-----------------------------
# Make a word frequency count
# normally hashes can be created using {} or just Hash.new
# but we want the default value of an entry to be 0 instead 
# of nil. (nil can't be incremented)
seen = Hash.new(0)
while (gets)
    gsub(/(\w[\w'-]*)/) { |word|
        seen[word.downcase] += 1
    }
end
# output hash in a descending numeric sort of its values
seen.sort { |a,b| b[1] <=> a[1] }.each do |k,v|
    printf("%5d %s\n", v, k )
end

#-----------------------------
# Line frequency count
seen = Hash.new(0)
while (gets)
    seen[$_.downcase] += 1
end
seen.sort { |a,b| b[1] <=> a[1] }.each do |k,v|
    printf("%5d %s\n", v, k )
end
#-----------------------------


# @@PLEAC@@_8.4
#-----------------------------
# instead of file handle FILE, we can just
# use a string containing the filename
File.readlines(file).each { |line|
    # do something with line
}
#-----------------------------
File.readlines(file).reverse_each { |line|
    # do something with line
}
#-----------------------------
# the variable lines might have been created
# this way
# lines = File.readlines(file)
#
# normally one would use the reverse_each, but
# if you insist on using a numerical index to
# iterate over the lines array...
(lines.size - 1).downto(0) { |i|
    line = lines[i]
}
#-----------------------------
# the second readlines argument is a the 
# record separator $/, just like perl, a blank
# separator splits the records into paragraphs
File.readlines(file, '').each { |paragraph|
    # do something with paragraph
    puts "->Paragraph #{paragraph}"
}
#-----------------------------


# @@PLEAC@@_8.6

$/ = "%\n";
srand;

File.open('/usr/share/fortune/humorists').each do |line|
    adage = line if rand($.) < 1
end

puts adage;


# @@PLEAC@@_8.10
begin
    fh = File.open(file, "r+")
    addr = fh.tell unless fh.eof while fh.gets
    fh.truncate(addr)
rescue SystemCallError
    $stderr.puts "#$!"
end


# @@PLEAC@@_9.0
entry = File.stat("/usr/bin/vi")
entry = File.stat("/usr/bin")
entry = File.stat(INFILE)

entry = File.stat("/usr/bin/vi")
ctime = entry.ctime
size  = entry.size

f = File.open(filename, "r")

## There is no -T equivalent in Ruby, but we can still test emptiness
if test(?s, filename)
  puts "#{filename} doesn't have text in it."
  exit
end

Dir.new("/usr/bin").each do |filename|
  puts "Inside /usr/bin is something called #{filename}"
end


# @@PLEAC@@_9.1
file = File.stat("filename")
readtime, writetime = file.atime, file.mtime
file.utime(readtime, writetime)  

SECONDS_PER_DAY = 60 * 60 * 24
file = File.stat("filename")
atime, mtime = file.atime, file.mtime

atime -= 7 * SECONDS_PER_DAY
mtime -= 7 * SECONDS_PER_DAY

File.utime(atime, mtime, file)
mtime = File.stat(file).mtime
File.utime(Time.new, mtime, file)
File.utime(Time.new, File.stat("testfile").mtime, file)

#-----------------------------
#!/usr/bin/ruby -w
## uvi - vi a file without changing it's access times

if ARGV.length != 1
  puts "usage: uvi filename"
  exit
end
file = ARGV[0]
atime, mtime = File.stat(file).atime, File.stat(file).mtime
system(ENV["EDITOR"] || "vi", file)
File.utime(atime, mtime, file)
#-----------------------------


# @@PLEAC@@_9.2
File.unlink(FILENAME)

err_flg = false
filenames.each do |file|
  begin
    File.unlink(file)
  rescue
    err_flg = $!
  end
end
err_flg and raise "Couldn't unlink all of #{filenames.join(" ")}: #{err_flg}"

File.unlink(file)

count = filenames.length
filenames.each do |file|
  begin
    File.unlink(file)
  rescue
    count -= 1
  end
end
if count != filenames.length
  STDERR.puts "could only delete #{count} of #{filenames.length} files"
end


# @@PLEAC@@_9.3
require "ftools"
File.copy(oldfile, newfile)

infile  = File.open(oldfile, "r")
outfile = File.open(newfile, "w")

blksize = infile.stat.blksize
# This doesn't handle partial writes or ^Z
# like the Perl version does.
while (line = infile.read(blksize))
  outfile.write(line)
end

infile.close
outfile.close

system("cp #{oldfile} #{newfile}")    # unix
system("copy #{oldfile} #{newfile}")  # dos, vms

require "ftools"
File.copy("datafile.dat", "datafile.bak")
File.move("datafile.new", "datafile.dat")


# @@PLEAC@@_9.4
$seen = {} # must use global var to be seen inside of method below

def do_my_thing(filename)
    dev, ino = File.stat(filename).dev, File.stat(filename).ino
    unless $seen[[dev, ino]]
        # do something with $filename because we haven't
        # seen it before
    end
    $seen[[dev, ino]] = $seen[[dev, ino]].to_i + 1
end

files.each do |filename|
    dev, ino = File.stat(filename).dev, File.stat(filename).ino
    if !$seen.has_key?([dev, ino])
        $seen[[dev, ino]] = []
    end
    $seen[[dev, ino]].push(filename)
end

$seen.keys.sort.each do |devino|
    ino, dev = devino
    if $seen[devino].length > 1
        # $seen[devino] is a list of filenames for the same file
    end
end


# @@PLEAC@@_9.5
Dir.open(dirname) do |dir|
    dir.each do |file|
        # do something with dirname/file
        puts file
    end
end
# Dir.close is automatic

# No -T equivalent in Ruby

dir.each do |file|
    next if file =~ /^\.\.?$/
    # ...
end

def plainfiles(dir)
    dh = Dir.open(dir)
    dh.entries.grep(/^[^.]/).
        map      {|file| "#{dir}/#{file}"}.
        find_all {|file| test(?f, file)}.
        sort
end


# @@PLEAC@@_9.6
list = Dir.glob("*.c")

dir = Dir.open(path)
files = dir.entries.grep(/\.c$/)
dir.close

files = Dir.glob("*.c")
files = Dir.open(path).entries.grep(/\.[ch]$/i)

dir = Dir.new(path)
files = dir.entries.grep(/\.[ch]$/i)

begin
  d = Dir.open(dir)
rescue Errno::ENOENT
  raise "Couldn't open #{dir} for reading: #{$!}"
end

files = []
d.each do |file|
  puts file
  next unless file =~ /\.[ch]$/i

  filename = "#{dir}/#{file}"
  # There is no -T equivalent in Ruby, but we can still test emptiness
  files.push(filename) if test(?s, filename)
end

dirs.entries.grep(/^\d+$/).
             map    { |file| [file, "#{path}/#{file}"]} .
             select { |file| test(?d, file[1]) }.
             sort   { |a,b|  a[0] <=> b[0] }.
             map    { |file| file[1] }


# @@PLEAC@@_9.7
require 'find'
Find.find(dirlist) do |file|
  # do whatever
end

require 'find'
argv = ARGV.empty? ? %w{.} : ARGV
Find.find(*argv) do |file|
  print file, (test(?d, file) ? "/\n" : "\n")
end

require 'find'
argv = ARGV.empty? ? %w{.} : ARGV
sum = 0
Find.find(*argv) do |file|
  size = test(?s, file) || 0
  sum += size
end
puts "#{argv.join(' ')} contains #{sum} bytes"

require 'find'
argv = ARGV.empty? ? %w{.} : ARGV
saved_size, saved_name = -1, ""
Find.find(*argv) do |file|
  size = test(?s, file) || 0
  next unless test(?f, file) && size > saved_size
  saved_size = size
  saved_name = file
end
puts "Biggest file #{saved_name} in #{argv.join(' ')} is #{saved_size}"

require 'find'
argv = ARGV.empty? ? %w{.} : ARGV
age, name = nil
Find.find(*argv) do |file|
  mtime = File.stat(file).mtime
  next if age && age > mtime
  age = mtime
  name = file
end
puts "#{name} #{age}"

#-----------------------------
#!/usr/bin/ruby -w
# fdirs - find all directories
require 'find'
argv = ARGV.empty? ? %w{.} : ARGV
File.find(*argv) { |file| puts file if test(?d, file) }
#-----------------------------


# @@PLEAC@@_9.8
require 'fileutils'

puts "Usage #{$0} dir ..." if ARGV.empty?
ARGV.each do |dir|
  FileUtils.rmtree(dir)
end


# @@PLEAC@@_9.9
require 'ftools'
names.each do |file|
  newname = file
  begin
    File.move(file, newname)
  rescue Errno::EPERM
    $stderr.puts "Couldn't rename #{file} to #{newname}: #{$!}"
  end
end

require 'ftools'
op = ARGV.empty? ? (raise "Usage: rename expr [files]\n") : ARGV.shift
argv = ARGV.empty? ? $stdin.readlines.map { |f| f.chomp } : ARGV
argv.each do |file|
  was = file
  file = eval("file.#{op}")
  File.move(was, file) unless was == file
end


# @@PLEAC@@_9.10
base = File.basename(path)
dir  = File.dirname(path)
# ruby has no fileparse equivalent
dir, base = File.split(path)
ext = base.scan(/\..*$/).to_s

path = '/usr/lib/libc.a'
file = File.basename(path)
dir  = File.dirname(path)

puts "dir is #{dir}, file is #{file}"
# dir is /usr/lib, file is libc.a

path = '/usr/lib/libc.a'
dir, filename = File.split(path)
name, ext = filename.split(/(?=\.)/)
puts "dir is #{dir}, name is #{name}, ext is #{ext}"
#   NOTE: The Ruby code prints
#   dir is /usr/lib, name is libc, extension is .a
#     while the Perl code prints a '/' after the directory name
#   dir is /usr/lib/, name is libc, extension is .a

# No fileparse_set_fstype() equivalent in ruby

def extension(path)
    ext = path.scan(/\..*$/).to_s
    ext.sub(/^\./, "")
end


# @@PLEAC@@_9.11
#-----------------------------
#!/usr/bin/ruby -w
# symirror - build spectral forest of symlinks

require 'find'
require 'fileutils'

raise "usage: #{$0} realdir mirrordir" unless ARGV.size == 2

srcdir,dstdir = ARGV
srcmode = File::stat(srcdir).mode
Dir.mkdir(dstdir, srcmode & 07777) unless test(?d, dstdir)

# fix relative paths
Dir.chdir(srcdir) {srcdir = Dir.pwd}
Dir.chdir(dstdir) {dstdir = Dir.pwd}

Find.find(srcdir) do |srcfile| 
    if test(?d, srcfile)
        dest = srcfile.sub(/^#{srcdir}/, dstdir)
        dmode = File::stat(srcfile).mode & 07777
        Dir.mkdir(dest, dmode) unless test(?d, dest)
        a = Dir["#{srcfile}/*"].reject{|f| test(?d, f)}
        FileUtils.ln_s(a, dest)
    end
end


# @@PLEAC@@_9.12
# we use the Getopt/Declare library here for convenience:
#   http://raa.ruby-lang.org/project/getoptdeclare/
#-----------------------------
#!/usr/bin/ruby -w
# lst - list sorted directory contents (depth first)

require 'find'
require 'etc'
require "Getopt/Declare"

# Note: in the option-spec below there must by at least one hard
# tab in between each -option and its description. For example
#    -i <tab> read from stdin

opts = Getopt::Declare.new(<<'EOPARAM')
    ============
    Input Format:
        -i	read from stdin
    ============
    Output Format:
        -l	long listing
        -r	reverse listing
    ============
    Sort on: (one of)
        -m	mtime (modify time - default)
                {$sort_criteria = :mtime}
        -u	atime (access time)
                {$sort_criteria = :atime}
        -c	ctime (inode change time)
                {$sort_criteria = :ctime}
        -s	size
                {$sort_criteria = :size}
        [mutex: -m -u -c -s]

EOPARAM

$sort_criteria ||= :mtime
files = {}
DIRS = opts['-i'] ? $stdin.readlines.map{|f|f.chomp!} : ARGV
DIRS.each do |dir|
    Find.find(dir) do |ent|
        files[ent] = File::stat(ent)
    end
end
entries = files.keys.sort_by{|f| files[f].send($sort_criteria)}
entries = entries.reverse unless opts['-r']

entries.each do |ent|
    unless opts['-l']
        puts ent
        next
    end
    stats = files[ent]
    ftime = stats.send($sort_criteria == :size ? :mtime : $sort_criteria)
    printf "%6d %04o %6d %8s %8s %8d %s %s\n",
        stats.ino,
        stats.mode & 07777,
        stats.nlink,
        ETC::PASSWD[stats.uid].name,
        ETC::GROUP[stats.gid].name,
        stats.size,
        ftime.strftime("%a %b %d %H:%M:%S %Y"),
        ent
end


# @@PLEAC@@_10.0
def hello
    $greeted += 1      # in Ruby, a variable beginning with $ is global (can be any type of course)
    puts "hi there!"
end

# We need to initialize $greeted before it can be used, because "+=" is waiting a Numeric object
$greeted = 0
hello                  # note that appending () is optional to function calls with no parameters


# @@PLEAC@@_10.1
# In Ruby, parameters are named anyway
def hypotenuse(side1, side2)
    Math.sqrt(side1**2 + side2**2)    # the sqrt function comes from the Math module
end
diag = hypotenuse(3, 4)

puts hypotenuse(3, 4)

a = [3, 4]
print hypotenuse(*a)                  # the star operator will magically convert an Array into a "tuple"

both = men + women

# In Ruby, all objects are references, so the same problem arises; we then return a new object
nums = [1.4, 3.5, 6.7]
def int_all(n)
    n.collect { |v| v.to_i }
end
ints = int_all(nums)

nums = [1.4, 3.5, 6.7]
def trunc_em(n)
    n.collect! { |v| v.to_i }         # the bang-version of collect modifies the object
end
trunc_em(nums)

# Ruby has two chomp version:
# ``chomp'' chomps the record separator and returns what's expected
# ``chomp!'' does the same but also modifies the parameter object


# @@PLEAC@@_10.2
def somefunc
    variable = something  # variable is local by default
end

name, age = ARGV
start     = fetch_time

a, b = pair               # will succeed if pair is an Array object (like ARGV is)
c = fetch_time

# In ruby, run_check can't access a, b, or c until they are
# explicitely defined global (using leading $), even if they are
# both defined in the same scope

def check_x(x)
    y = "whatever"
    run_check
    if $condition
        puts "got $x"
    end
end

# The following will keep a reference to the array, though the
# results will be slightly different from perl: the last element
# of $global_array will be itself an array
def save_array(ary)
    $global_array << ary
end

# The following gives the same results as in Perl for $global_array,
# though it doesn't illustrate anymore the way to keep a reference
# to an object: $global_array is extended with the elements of ary
def save_array(ary)
    $global_array += ary
end


# @@PLEAC@@_10.3
# In Ruby, AFAIK a method cannot access "local variables" defined
# upper scope; mostly because everything is an object, so you'll
# do the same by defining an attribute or a static attribute

# In Ruby the BEGIN also exists:
BEGIN { puts "hello from BEGIN" }
puts "hello from main"
BEGIN { puts "hello from 2nd BEGIN" }
# gives:
#   hello from BEGIN
#   hello from 2nd BEGIN
#   hello from main

# In Ruby, it can be written as a static method and a static
# variable
class Counter
    @@counter = 0
    def Counter.next_counter; @@counter += 1; end
end

# There is no need of BEGIN since the variable will get
# initialized when parsing
class Counter
    @@counter = 42
    def Counter.next_counter; @@counter += 1; end
    def Counter.prev_counter; @@counter -= 1; end
end


# @@PLEAC@@_10.4
# You can either get the whole trace as an array of strings, each
# string telling which file, line and method is calling:
caller

# ...or only the last caller
caller[0]

# We need to extract just the method name of the backtrace:
def whoami;  caller()[0] =~ /in `([^']+)'/ ? $1 : '(anonymous)'; end
def whowasi; caller()[1] =~ /in `([^']+)'/ ? $1 : '(anonymous)'; end


# @@PLEAC@@_10.5
# In Ruby, every value is a reference on an object, thus there is
# no such problem
array_diff(array1, array2)

def add_vecpair(a1, a2)
    results = []
    a1.each_index { |i| results << (a1[i] + a2[i]) }
    results
end
a = [1, 2]
b = [5, 8]
c = add_vecpair(a, b)
p c

# Add this to the beginning of the function to check if we were
# given two arrays
a1.type == Array && a2.type == Array or
    raise "usage: add_vecpair array1 array2 (was used with: #{a1.type} #{a2.type})"


# @@PLEAC@@_10.6
# There is no return context in Ruby


# @@PLEAC@@_10.7
# Like in Perl, we need to fake with a hash, but it's dirty :-(
def thefunc(param_args)
    args = { 'INCREMENT' => '10s', 'FINISH' => '0', 'START' => 0 }
    args.update(param_args)
    if (args['INCREMENT']  =~ /m$/ )
        # .....
    end
end

thefunc({ 'INCREMENT' => '20s', 'START' => '+5m', 'FINISH' => '+30m' })
thefunc({})


# @@PLEAC@@_10.8
# there is no "undef" direct equivalent but there is the slice equiv:
a, c = func.indexes(0, 2)


# @@PLEAC@@_10.9
# Ruby has no such limitation:
def somefunc
    ary = []
    hash = {}
    # ...
    return ary, hash
end
arr, dict = somefunc

array_of_hashes = fn
h1, h2, h3      = fn


# @@PLEAC@@_10.10
return
# or (equivalent)
return nil


# @@PLEAC@@_10.11
# You can't prototype in Ruby regarding types :-(
# Though, you can force the number of arguments:
def func_with_no_arg; end
def func_with_no_arg(); end
def func_with_one_arg(a1); end
def func_with_two_args(a1, a2); end
def func_with_any_number_of_args(*args); end


# @@PLEAC@@_10.12
raise "some message"        # raise exception

begin
    val = func
rescue Exception => msg
    $stderr.puts "func raised an exception: #{msg}"
end

# In Ruby the rescue statement uses an exception class, every
# exception which is not matched is still continuing
begin
    val = func
rescue FullMoonError
    ...
end


# @@PLEAC@@_10.13
# Saving Global Values
# Of course we can just save the value and restore it later:
def print_age
    puts "Age is #{$age}"
end

$age = 18         # global variable
print_age()
if condition
    safeage = $age
    $age = 23
    print_age()
    $age = safeage
end

# We can also use a method that saves the global variable and
# restores it automatically when the block is left:

def local(var)
    eval("save = #{var.id2name}")
    begin
        result = yield
    ensure
        # we want to call this even if we got an exception
        eval("#{var.id2name} = save")
    end
    result
end

condition = true
$age = 18
print_age()
if condition
    local(:$age) {
        $age = 23
        print_age()
    }
end
print_age()

# There is no need to use local() for filehandles or directory
# handles in ruby because filehandles are normal objects.


# @@PLEAC@@_10.14
# In Ruby you may redefine a method [but not overload it :-(]
# just by defining again with the same name.
def foo; puts 'foo'; end
def foo; puts 'bar'; end
foo
#=> bar

# You can also take a reference to an existing method before
# redefining a new one, using the `alias' keyword
def foo; puts 'foo'; end
alias foo_orig foo
def foo; puts 'bar'; end
foo_orig
foo
#=> foo
#=> bar

# AFAIK, there is no direct way to create a new method whose name
# comes from a variable, so use "eval"
colors = %w(red blue green yellow orange purple violet)
colors.each { |c|
    eval <<-EOS
    def #{c}(*a)
        "<FONT COLOR='#{c}'>" + a.to_s + "</FONT>"
    end
    EOS
} 


# @@PLEAC@@_10.15
def method_missing(name, *args)
    "<FONT COLOR='#{name}'>" + args.join(' ') + "</FONT>"
end
puts chartreuse("stuff")


# @@PLEAC@@_10.16
def outer(arg)
    x = arg + 35
    inner = proc { x * 19 }
    x + inner.call()
end


# @@PLEAC@@_10.17
#!/usr/bin/ruby -w
# mailsort - sort mbox by different criteria
require 'English'
require 'Date'

# Objects of class Mail represent a single mail.
class Mail
    attr_accessor :no
    attr_accessor :subject
    attr_accessor :fulltext
    attr_accessor :date

    def initialize
        @fulltext = ""
        @subject = ""
    end

    def append(para)
        @fulltext << para
    end

    # this is called if you call puts(mail)
    def to_s
        @fulltext
    end
end

# represents a list of mails.
class Mailbox < Array

    Subjectpattern = Regexp.new('Subject:\s*(?:Re:\s*)*(.*)\n')
    Datepattern = Regexp.new('Date:\s*(.*)\n')

    # reads mails from open file and stores them
    def read(file)
        $INPUT_RECORD_SEPARATOR = ''  # paragraph reads
        msgno = -1
        file.each { |para|
            if para =~ /^From/
                mail = Mail.new
                mail.no = (msgno += 1)
                md = Subjectpattern.match(para)
                if md
                    mail.subject = md[1]
                end
                md = Datepattern.match(para)
                if md
                    mail.date = DateTime.parse(md[1])
                else
                    mail.date = DateTime.now
                end
                self.push(mail)
            end
            mail.append(para) if mail
        }
    end

    def sort_by_subject_and_no
        self.sort_by { |m|
            [m.subject, m.no]
        }
    end

    # sorts by a list of attributs of mail, given as symbols
    def sort_by_attributs(*attrs)
        # you can sort an Enumerable by an array of
        # values, they would be compared
        # from ary[0] to ary[n]t, say:
        # ['b',1] > ['a',10] > ['a',9]
        self.sort_by { |elem|
            attrs.map { |attr|
                elem.send(attr)
            }
        }
    end

end

mailbox = Mailbox.new
mailbox.read(ARGF)

# print only subjects sorted by subject and number
for m in mailbox.sort_by_subject_and_no
    puts(m.subject)
end

# print complete mails sorted by date, then subject, then number
for m in mailbox.sort_by_attributs(:date, :subject)
    puts(m)
end


# @@PLEAC@@_11.7
def mkcounter(count)
    start  = count 
    bundle = { 
        "NEXT"   => proc { count += 1 },
        "PREV"   => proc { count -= 1 },
        "RESET"  => proc { count = start }
    }
    bundle["LAST"] = bundle["PREV"]
    return bundle
end

c1 = mkcounter(20)
c2 = mkcounter(77)

puts "next c1: #{c1["NEXT"].call}"  # 21 
puts "next c2: #{c2["NEXT"].call}"  # 78 
puts "next c1: #{c1["NEXT"].call}"  # 22 
puts "last c1: #{c1["PREV"].call}"  # 21 
puts "last c1: #{c1["LAST"].call}"  # 20 
puts "old  c2: #{c2["RESET"].call}" # 77 


# @@PLEAC@@_11.15
class Binary_tree
    def initialize(val)
        @value = val
        @left = nil
        @right = nil
    end
    
    # insert given value into proper point of
    # provided tree.  If no tree provided, 
    # use implicit pass by reference aspect of @_
    # to fill one in for our caller.
    def insert(val)
        if val < @value then
            if @left then
                @left.insert(val)
            else
                @left = Binary_tree.new(val)
            end
        elsif val > @value then
            if @right then
                @right.insert(val)
            else
                @right = Binary_tree.new(val)
            end
        else
            puts "double"
            # do nothing, no double values
        end
    end

    # recurse on left child, 
    # then show current value, 
    # then recurse on right child.  
    def in_order
        @left.in_order if @left
        print @value, " "
        @right.in_order if @right
    end
    
    # show current value, 
    # then recurse on left child, 
    # then recurse on right child.
    def pre_order
        print @value, " "
        @left.pre_order if @left
        @right.pre_order if @right
    end

    # recurse on left child, 
    # then recurse on right child,
    # then show current value.
    def post_order
        @left.post_order if @left
        @right.post_order if @right
        print @value, " "
    end

    # find out whether provided value is in the tree.
    # if so, return the node at which the value was found.
    # cut down search time by only looking in the correct
    # branch, based on current value.
    def search(val)
        if val == @value then
            return self
        elsif val < @value then
            return @left.search(val) if @left
            return nil
        else
            return @right.search(val) if @right
            return nil
        end
    end
end

# first generate 20 random inserts
test = Binary_tree.new(0)
for a in 0..20
    test.insert(rand(1000)) 
end

# now dump out the tree all three ways
print "Pre order:  ";  test.pre_order;  puts ""
print "In order:  ";  test.in_order;  puts ""
print "Post order:  ";  test.post_order;  puts ""

print "search?"
while gets
    print test.search($_.to_i)
    print "\nsearch?"
end


# @@PLEAC@@_12.0
# class and module names need to have the first letter capitalized
module Alpha
    NAME = 'first'
end
module Omega
    NAME = 'last'
end
puts "Alpha is #{Alpha::NAME}, Omega is #{Omega::NAME}"

# ruby doesn't differentiate beteen compile-time and run-time
require 'getoptlong.rb'
require 'getoptlong'     # assumes the .rb
require 'cards/poker.rb'
require 'cards/poker'    # assumes the .rb
load    'cards/poker'    # require only loads the file once

module Cards
    module Poker
        @card_deck = Array.new # or @card_deck = []
        def shuffle
        end
    end
end


# @@PLEAC@@_12.1
# a module exports all of its functions
module Your_Module
    def self.function
        # this would be called as Your_Module.function
    end
    
    def Your_Module.another
        # this is the same as above, but more specific
    end
end

# @@PLEAC@@_12.2
begin
    require 'nonexistent'
rescue LoadError
    puts "Couldn't load #{$!}"  # $! contains the last error string
end

# @@PLEAC@@_12.4
# module variables are private unless access functions are defined
module Alpha
    @aa = 10
    @bb = 11
    
    def self.put_aa
        puts @aa
    end
    
    def self.bb=(val)
        @bb = val
    end
end

Alpha.bb = 12
# Alpha.aa = 10 # error, no aa=method


# @@PLEAC@@_12.5
# caller provides a backtrace of the call stack
module MyModule
    def find_caller
        caller
    end

    def find_caller2(i)
        caller(i) # an argument limits the size of the stack returned
    end
end


# @@PLEAC@@_12.6
BEGIN {
    $logfile = '/tmp/mylog' unless defined? $logfile
    $LF = File.open($logfile, 'a')
}

module Logger
    def self.logmsg(msg)
        $LF.puts msg
    end

    logmsg('startup')
end

END {
    Logger::logmsg('shutdown')
    $LF.close
}


# @@PLEAC@@_12.7
#-----------------------------
# results may be different on your system
# % ruby -e "$LOAD_PATH.each_index { |i| printf("%d %s\n", i, $LOAD_PATH[i] }
#0 /usr/local/lib/site_ruby/1.6
#1 /usr/local/lib/site_ruby/1.6/i386-linux
#2 /usr/local/lib/site_ruby/
#3 /usr/lib/ruby/1.6
#4 /usr/lib/ruby/1.6/i136-linux
#5 .
#-----------------------------
# syntax for sh, bash, ksh, or zsh
#$ export RUBYLIB=$HOME/rubylib

# syntax for csh or tcsh
# % setenv RUBYLIB ~/rubylib
#-----------------------------
$LOAD_PATH.unshift "/projects/spectre/lib";


# @@PLEAC@@_12.8
# equivalents in ruby are mkmf, SWIG, or Ruby/DL depending on usage


# @@PLEAC@@_12.9
# no equivalent in ruby


# @@PLEAC@@_12.10
# no equivalent in ruby


# @@PLEAC@@_12.11
module FineTime
    def self.time
        # to be defined later
    end
end


module FineTime
    def self.time
        "its a fine time"
    end
end

puts FineTime.time #=> "its a fine time"


# @@PLEAC@@_12.12
def even_only(n)
    raise "#{n} is not even" if (n & 1) != 0  # one way to test
    # ...
end
def even_only(n)
    $stderr.puts "#{n} is not even" if (n & 1) != 0
    # ...
end


# @@PLEAC@@_12.17
# The library archive for ruby is called Ruby Application archive,
# or shorter RAA, and can be found at http://raa.ruby-lang.org.
# A typical library is installed like this:
# % gunzip some-module-4.54.tar.gz
# % tar xf some-module-4.54.tar
# % cd some-module-4.54.tar
# % ruby install.rb config
# % ruby install.rb setup
# get superuser previleges here if needed for next step
# % ruby install.rb install

# Some modules use a different process,
# you should find details in the documentation
# Here is an example of such a different process
# % ruby extconf.rb
# % make
# % make install

# If you want the module installed in your own directory:
# For ruby version specific libraries
# % ruby install.rb config --site-ruby=~/lib
# For version independent libraries
# % ruby install.rb config --site-ruby-common=~/lib

# Information about possible options for config
# % ruby install.rb --help

# If you have your own complete distribution
# % ruby install.rb --prefix=path=~/ruby-private


# @@PLEAC@@_13.0
# Classes and objects in Ruby are rather straigthforward
class Person
    # Class variables (also called static attributes) are prefixed by @@
    @@person_counter=0
    
    # object constructor
    def initialize(age, name, alive = true)     # Default arg like in C++
        @age, @name, @alive = age, name, alive  # Object attributes are prefixed by '@'
        @@person_counter += 1
          # There is no '++' operator in Ruby. The '++'/'--'  operators are in fact 
          # hidden assignments which affect variables, not objects. You cannot accomplish
          # assignment via method. Since everything in Ruby is object, '++' and '--' 
          # contradict Ruby OO ideology. Instead '-=' and '+=' are used.
    end
    
    attr_accessor :name, :age   # This creates setter and getter methods for @name
                                # and @age. See 13.3 for detailes.
    
    # methods modifying the receiver object usually have the '!' suffix
    def die!
        @alive = false
        puts "#{@name} has died at the age of #{@age}."
        @alive
    end
    
    def kill(anotherPerson)
        print @name, ' is killing ', anotherPerson.name, ".\n"
        anotherPerson.die!
    end

    # methods used as queries
    # usually have the '?' suffix    
    def alive?
        @alive && true
    end
    
    def year_of_birth
        Time.now.year - @age
    end
    
    # Class method (also called static method)
    def Person.number_of_people
        @@person_counter
    end
end

# Using the class:
# Create objects of class Person
lecter = Person.new(47, 'Hannibal')
starling = Person.new(29, 'Clarice', true)
pazzi = Person.new(40, 'Rinaldo', true)

# Calling a class method
print "There are ", Person.number_of_people, " Person objects\n"

print pazzi.name, ' is ', (pazzi.alive?) ? 'alive' : 'dead', ".\n"
lecter.kill(pazzi)
print pazzi.name, ' is ', (pazzi.alive?) ? 'alive' : 'dead', ".\n"

print starling.name , ' was born in ', starling.year_of_birth, "\n"


# @@PLEAC@@_13.1
# If you don't need any initialisation in the constructor,
# you don't need to write a constructor.
class MyClass
end

class MyClass
    def initialize
        @start = Time.new
        @age = 0
    end
end

class MyClass
    def initialize(inithash)
        @start = Time.new
        @age = 0
        for key, value in inithash
            instance_variable_set("@#{key}", value)
        end
    end
end

# @@PLEAC@@_13.2
# Objects are destroyed by the garbage collector.
# The time of destroying is not predictable.
# The ruby garbage collector can handle circular references,
# so there is no need to write destructor for that.

# There is no direct support for destructor.
# You can call a custom function, or more specific a proc object, when the
# garbage collector is about to destruct the object, but it is unpredictable
# when this occurs.
# Also if such a finalizer object has a reference to the orignal object,
# this may prevent the original object to get garbage collected.
# Because of this problem the finalize method below is
# a class method and not a instance method.
# So if you need to free resources for an object, like
# closing a socket or kill a spawned subprocess,
# you should do it explicitly.

class MyClass
    def initialize
        ObjectSpace.define_finalizer(self,
                                     self.class.method(:finalize).to_proc)
    end
    def MyClass.finalize(id)
        puts "Object #{id} dying at #{Time.new}"
    end
end

# test code
3.times {
    MyClass.new
}
ObjectSpace.garbage_collect


# @@PLEAC@@_13.3
# You can write getter and setter methods in a natural way:
class Person
    def name
        @name
    end
    def name=(name)
        @name = name
    end
end

# But there is a better and shorter way
class Person
    attr_reader :age
    attr_writer :name  
    # attr_reader and attr_writer are actually methods in class Class
    # which set getter and setter methods for you.
end

# There is also attr_accessor to create both setters and getters
class Person
    attr_accessor :age, :name
end


# @@PLEAC@@_13.4
class Person
    # Class variables (also called static attributes) are prefixed by @@
    @@person_counter = 0
    
    def Person.population
        @@person_counter
    end
    def initialize
        @@person_counter += 1
        ObjectSpace.define_finalizer(self,
                                     self.class.method(:finalize).to_proc)
    end
    def Person.finalize(id)
        @@person_counter -= 1
    end
end
people = []
10.times {
    people.push(Person.new)
}
printf("There are %d people alive", Person.population)


FixedArray.class_max_bounds = 100
alpha = FixedArray.new
puts "Bound on alpha is #{alpha.max_bounds}"

beta = FixedArray.new
beta.max_bounds = 50                    # calls the instance method
beta.class.class_max_bounds = 50        # alternative, calls the class method
puts "Bound on alpha is #{alpha.max_bounds}"
    
class FixedArray
    @@bounds = 7
    
    def max_bounds
        @@max_bounds
    end
    # instance method, which sets the class variable
    def max_bounds=(value)
        @@max_bounds = value
    end
    # class method. This can only be called on a class,
    # but not on the instances
    def FixedArray.class_max_bounds=(value)
        @@max_bounds = value
    end
end


# @@PLEAC@@_13.5
PersonStruct = Struct.new("Person", :name, :age, :peers)
# creates a class "Person::Struct", which is accessiable with the
# constant "PersonStruct"
p = PersonStruct.new
p = Struct::Person.new                      # alternative using the classname
p.name = "Jason Smythe"
p.age = 13
p.peers = ["Wilbur", "Ralph", "Fred"]
p[:peers] = ["Wilbur", "Ralph", "Fred"]     # alternative access using symbol
p["peers"] = ["Wilbur", "Ralph", "Fred"]    # alternative access using name of field
p[2] = ["Wilbur", "Ralph", "Fred"]          # alternative access using index of field
puts "At age #{p.age}, #{p.name}'s first friend is #{p.peers[0]}"

# The fields of a struct have no special type, like other ruby variables
# you can put any objects in. Therefore the discussions how to specify
# the types of the fields do not apply to ruby.

FamilyStruct = Struct.new("Family", :head, :address, :members)
folks = FamilyStruct.new
folks.head = PersonStruct.new
dad = folks.head
dad.name = "John"
dad.age = 34

# supply of own accessor method for the struct for error checking
class PersonStruct
    def age=(value)
        if !value.kind_of?(Integer)
            raise(ArgumentError, "Age #{value} isn't an Integer")
        elsif value > 150
            raise(ArgumentError, "Age #{value} is unreasonable")
        end
        @age = value
    end
end


# @@PLEAC@@_13.6
# The ruby Object class defines a dup and a clone method.
# The dup method is recommended for prototype object creation.
# The default implementation makes a shallow copy,
# but each class can override it, for example to make a deep copy.

# If you want to call 'new' directly on the instances,
# you can create a instance method "new", which returns a new duplicate.
# This method is distinct from the class method new.
#
class A
    def new
        dup
    end
end

ob1 = A.new
# later on
ob2 = ob1.new


# @@PLEAC@@_13.7
methname = 'flicker'
obj.send(methname, 10)      # calls obj.flicker(10)

# call three methods on the object, by name
['start', 'run', 'stop'].each do |method_string|
    obj.send(method_string)
end

# Another way is to create a Method object
method_obj = obj.method('flicker')
# And then call it
method_obj.call(10)


# @@PLEAC@@_13.8
# All classes in Ruby inherit from class Object
# and thus all objects share methods defined in this class

# the class of the object
puts any_object.type

# Ruby classes are actually objects of class Class and they
# respond to methods defined in Object class as well

# the superclass of this class
puts any_object.class.superclass

# ask an object whether it is an instance of particular class
n = 4.7
puts n.instance_of?(Float)    # true
puts n.instance_of?(Numeric)  # false

# ask an object whether it is an instance of class, one of the
# superclasses of the object, or modules included in it
puts n.kind_of?(Float)       # true (the class)
puts n.kind_of?(Numeric)     # true (an ancestor class)
puts n.kind_of?(Comparable)  # true (a mixin module)
puts n.kind_of?(String)      # false

# ask an object whether it can respond to a particular method
puts n.respond_to?('+')      # true
puts n.respond_to?('length') # false

# all methods an object can respond to
'just a string'.methods.each { |m| puts m }


# @@PLEAC@@_13.9
# Actually any class in Ruby is inheritable
class Person        
    attr_accessor :age, :name
    def initialize
        @name
        @age
    end
end
#-----------------------------
dude = Person.new
dude.name = 'Jason'
dude.age = 23
printf "%s is age %d.\n", dude.name, dude.age
#-----------------------------
# Inheriting from Person
class Employee < Person
    attr_accessor :salary
end
#-----------------------------
empl = Employee.new
empl.name = 'Jason'
empl.age = 23
empl.salary = 200
printf "%s is age %d, the salary is %d.\n", empl.name, empl.age, empl.salary
#-----------------------------
# Any built-in class can be inherited the same way
class WeirdString < String  
    def initialize(obj)
        super obj
    end
    def +(anotherObj)   # + method in this class is overridden
        # to return the sum of string lengths
        self.length + anotherObj.length  # 'self' can be omitted
    end  
end
#-----------------------------
a = WeirdString.new('hello')
b = WeirdString.new('bye')

puts a + b    # the overridden +
#=> 8
puts a.length # method from the superclass, String
#=> 5


# @@PLEAC@@_13.11
# In ruby you can override the method_missing method
# to have a solution similar to perls AUTOLOAD.
class Person

    def initialize
        @ok_fields = %w(name age peers parent)
    end

    def valid_attribute?(name)
        @ok_fields.include?(name)
    end

    def method_missing(namesymbol, *params)
        name = namesymbol.to_s
        return if name =~ /^A-Z/
        if name.to_s[-1] == ('='[0])       # we have a setter
            isSetter = true
            name.sub!(/=$/, '')
        end
        if valid_attribute?(name)
            if isSetter
                instance_variable_set("@#{name}", *params)
            else
                instance_variable_get("@#{name}", *params)
            end
        else
            # if no annestor is responsible,
            # the Object class will throw a NoMethodError exception
            super(namesymbol, *params)
        end
    end

    def new
        kid = Person.new
        kid.parent = self
        kid
    end

end

dad = Person.new
dad.name = "Jason"
dad.age = 23
kid = dad.new
kid.name = "Rachel"
kid.age = 2
puts "Kid's parent is #{kid.parent.name}"
puts dad
puts kid

class Employee < Person
    def initialize
        super
        @ok_fields.push("salary", "boss")
    end
    def ok_fields
        @ok_fields
    end
end


# @@PLEAC@@_13.13
# The ruby garbage collector pretends to cope with circular structures.
# You can test it with this code:
class RingNode
    attr_accessor :next
    attr_accessor :prev
    attr_reader :name

    def initialize(aName)
        @name = aName
        ObjectSpace.define_finalizer(self,
                                     self.class.method(:finalize).to_proc)
    end

    def RingNode.finalize(id)
        puts "Node #{id} dying"
    end

    def RingNode.show_all_objects
        ObjectSpace.each_object {|id|
            puts id.name if id.class == RingNode
        }
    end
end

def create_test
    a = RingNode.new("Node A")
    b = RingNode.new("Node B")
    c = RingNode.new("Node C")
    a.next = b
    b.next = c
    c.next = a
    a.prev = c
    c.prev = b
    b.prev = a

    a = nil
    b = nil
    c = nil
end

create_test
RingNode.show_all_objects
ObjectSpace.garbage_collect
puts "After garbage collection"
RingNode.show_all_objects


# @@PLEAC@@_13.14
class String
    def <=>(other)
        self.casecmp other
    end
end

# There is no way to directly overload the '""' (stringify) 
# operator in Ruby.  However, by convention, classes which 
# can reasonably be converted to a String will define a 
# 'to_s' method as in the TimeNumber class defined below.
# The 'puts' method will automatcally call an object's
# 'to_s' method as is demonstrated below.
# Furthermore, if a class defines a to_str method, an object of that
# class can be used most any place where the interpreter is looking 
# for a String value.

#---------------------------------------
# NOTE: Ruby has a builtin Time class which would usually be used 
# to manipulate time objects, the following is supplied for
# educational purposes to demonstrate operator overloading.
#
class TimeNumber
    attr_accessor  :hours,:minutes,:seconds
    def initialize( hours, minutes, seconds)
        @hours = hours
        @minutes = minutes
        @seconds = seconds
    end
    
    def to_s
        return sprintf( "%d:%02d:%02d", @hours, @minutes, @seconds)
    end

    def to_str
        to_s
    end

    def +( other)
        seconds = @seconds + other.seconds
        minutes = @minutes + other.minutes
        hours = @hours + other.hours
        if seconds >= 60
            seconds %= 60
            minutes += 1
        end
        if minutes >= 60
            minutes %= 60
            hours += 1
        end
        return TimeNumber.new(hours, minutes, seconds)
    end

    def -(other)
        raise NotImplementedError
    end

    def *(other)
        raise NotImplementedError
    end

    def /( other)
        raise NotImplementedError
    end
end

t1 = TimeNumber.new(0, 58, 59)
sec = TimeNumber.new(0, 0, 1)
min = TimeNumber.new(0, 1, 0)
puts t1 + sec + min + min

#-----------------------------
# StrNum class example: Ruby's builtin String class already has the 
# capabilities outlined in StrNum Perl example, however the '*' operator
# on Ruby's String class acts differently: It creates a string which
# is the original string repeated N times.
#
# Using Ruby's String class as is in this example:
x = "Red"; y = "Black"
z = x+y
r = z*3 # r is "RedBlackRedBlackRedBlack"
puts "values are #{x}, #{y}, #{z}, and #{r}"
print "#{x} is ", x < y ? "LT" : "GE", " #{y}\n"
# prints:
# values are Red, Black, RedBlack, and RedBlackRedBlackRedBlack
# Red is GE Black

#-----------------------------
class FixNum
    REGEX = /(\.\d*)/
    DEFAULT_PLACES = 0
    attr_accessor :value, :places
    def initialize(value, places = nil)
        @value = value
        if places
            @places = places
        else
            m = REGEX.match(value.to_s) 
            if m
                @places = m[0].length - 1
            else
                @places = DEFAULT_PLACES
            end
        end
    end

    def +(other)
        FixNum.new(@value + other.value, max(@places, other.places))
    end

    def *(other)
        FixNum.new(@value * other.value, max(@places, other.places))
    end

    def /(other)
        puts "Divide: #{@value.to_f/other.value.to_f}"
        result = FixNum.new(@value.to_f/other.value.to_f)
        result.places = max(result.places,other.places) 
        result
    end

    def to_s
        sprintf("STR%s: %.*f", self.class.to_s , @places, @value)   #.
    end  

    def to_str
        to_s
    end

    def to_i #convert to int
        @value.to_i
    end

    def to_f #convert to float`
        @value.to_f
    end

    private
    def max(a,b)
        a > b ? a : b
    end
end

def demo()
    x = FixNum.new(40)
    y = FixNum.new(12, 0)

    puts "sum of #{x} and #{y} is  #{x+y}"
    puts "product of #{x} and #{y} is #{x*y}"

    z = x/y
    puts "#{z} has #{z.places} places"
    unless z.places
        z.places = 2
    end

    puts "div of #{x} by #{y} is #{z}"
    puts "square of that is  #{z*z}"
end

if __FILE__ == $0
    demo()
end


# @@PLEAC@@_14.1
# There are dbm, sdbm, gdbm modules
# and the bdb module for accessing the berkeley db
# sdbm seem to be available on the most systems,
# so we use it here
#
require "sdbm"
SDBM.open("filename", 0666) { |dbobj|
    # raises exception if open error
    
    # the returned sdbm-dbobj has most of the methods of a hash
    v = dbobj["key"]
    dbobj["key"] = "newvalue"
    if dbobj.has_key?("key")
        # ...
    end
    dbobj.delete("key2")
}
# database is open only inside the block.

# It is also possible to use a open .. close pair:
dbobj = SDBM.open("filename", 0666)
#.. do something with dbobj
dbobj.close

#!/usr/bin/ruby -w
# userstats - generate statistics on who is logged in
# call with usernames as argument to display the totals
# for the given usernames, call with "ALL" to display all users

require "sdbm"
filename = '/tmp/userstats.db'
SDBM.open(filename, 0666) { |dbobj|
    if ARGV.length > 0
        if ARGV[0] == "ALL"
            # ARGV is constant, so we need the variable userlist
            userlist = dbobj.keys().sort()
        else
            userlist = ARGV
        end
        userlist.each { |user|
            print "#{user}\t#{dbobj[user]}\n"
        }
    else
        who = `who`
        who.split("\n").each { |line|
            md = /^(\S+)/.match(line)
            raise "Bad line from who: #{line}" unless md
            # sdbm stores only strings, so "+=" doesn't work,
            # we need to convert them expicitly back to integer.
            if dbobj.has_key?(md[0])
                dbobj[md[0]] = dbobj[md[0]].to_i + 1
            else
                dbobj[md[0]] = "1"
            end
        }
    end
}


# @@PLEAC@@_14.2
# using open and clear
dbobj = SDBM.open("filename", 0666)
dbobj.clear()
dbobj.close()
# deleting file and recreating it
# the filenames depend on the flavor of dbm you use,
# for example sdbm has two files named filename.pag and filename.dir,
# so you need to delete both files
begin
    File.delete("filename")
    # raises Exception if not exist
    dbobj = SDBM.open("filename", 0666)
rescue
    # add error handling here
end


# @@PLEAC@@_14.3
# sdbm2gdbm: converts sdbm database to a gdbm database
require "sdbm"
require "gdbm"

unless ARGV.length == 2
    fail "usage: sdbm2gdbm infile outfile"
end
infile = ARGV[0]
outfile = ARGV[1]

sdb = SDBM.open(infile)
gdb = GDBM.open(outfile, 0666)
sdb.each { |key, val|
    gdb[key] = val
}
gdb.close
sdb.close


# @@PLEAC@@_14.4
#!/usr/bin/ruby -w
# dbmmerge: merges two dbm databases
require "sdbm"

unless ARGV.length == 3
    fail "usage: dbmmerge indb1 indb2 outdb"
end
infile1 = ARGV[0]
infile2 = ARGV[0]
outfile = ARGV[2]

in1 = SDBM.open(infile1, nil)
in2 = SDBM.open(infile2, nil)
outdb = SDBM.open(outfile, 0666)

[in1, in2].each { |indb|
    indb.each { |key, val|
        if outdb.has_key?(key)
            # decide which value to set.
            # set outdb[key] if necessary
        else
            outdb[key] = val
        end
    }
}
in1.close
in2.close
outdb.close


# @@PLEAC@@_14.7
# we write a tie method that extends the Array class.
# It reads the file into the memory, executes the code block
# in which you can manipulate the array as needed, and writes
# the array back to the file after the end of the block execution
class Array
    def tie(filename, flags)
        File.open(filename, flags) { |f|
            f.each_line { |line|
                self.push(line.chomp)
            }
            yield
            f.rewind
            each { |line|
                if line
                    f.puts(line)
                else
                    f.puts ""
                end
            }
        }
    end
end

array = Array.new
array.tie("/tmp/textfile.txt", File::RDWR|File::CREAT) {
    array[4] = "a new line 4"
}

# The tied array can be manipulated like a normal array,
# so there is no need for a special API, and the recno_demo program
# to demonstrate is API is useless


# tied array demo: show how to use array with a tied file
filename = "db_file.txt"
lines = Array.new
File.unlink(filename) if File.exists?(filename)
lines.tie(filename, File::RDWR | File::CREAT) {
    # first create a textfile to play with
    lines[0] = "zero"
    lines[1] = "one"
    lines[2] = "two"
    lines[3] = "three"
    lines[4] = "four"

    # print the records in order.
    # Opposed to perl, the tied array behaves exactly as a normal array
    puts "\nOriginal"
    for i in 0..(lines.length-1)
        puts "#{i}: #{lines[i]}"
    end

    #use push and pop
    a = lines.pop
    lines.push("last")
    puts("The last line was [#{a}]")

    #use shift and unshift
    a = lines.shift
    lines.unshift("first")
    puts("The first line was [#{a}]")

    # add record after record 2
    i = 2
    lines.insert(i + 1, "Newbie")

    # add record before record one
    i = 1
    lines.insert(i, "New One")

    # delete record 3
    lines.delete_at(3)

    #now print the records in reverse order
    puts "\nReverse"
    (lines.length - 1).downto(0){ |i|
        puts "#{i}: #{lines[i]}"
    }

}


# @@PLEAC@@_14.8
# example to store complex data in a database
# uses marshall from the standard library
require "sdbm"
db = SDBM.open("pleac14-8-database", 0666)

# convert the Objects into strings and back by using the Marshal module.
# Most normal objects can be converted out of the box,
# but not special things like procedure objects,
# IO instance variables, singleton objects

db["Tom Christiansen"] = Marshal.dump(["book author",  "tchrist@perl.com"])
db["Tom Boutell"] = Marshal.dump(["shareware author",
"boutell@boutell.com"])

name1 = "Tom Christiansen"
name2 = "Tom Boutell"

tom1 = Marshal.load(db[name1])
tom2 = Marshal.load(db[name2])

puts "Two Toming: #{tom1} #{tom2}"

if tom1[0] == tom2[0] && tom1[1] == tom2[1]
   puts "You're having runtime fun with one Tom made two."
else
   puts "No two Toms are ever alike"
end

# To change parts of an entry, get the whole entry, change the parts,
# and save the whole entry back
entry = Marshal.load(db["Tom Boutell"])
entry[0] = "Poet Programmer"
db["Tom Boutell"] = Marshal.dump(entry)
db.close


# @@PLEAC@@_14.9
# example to make data persistent
# uses Marshal from the standard lib
# Stores the data in a simple file,
# see 14.8 on how to store it in a dbm file

# The BEGIN block is executed before the rest of the script
# we use global variables here because local variables
# will go out of scope and are not accessible from the main script

BEGIN {
   $persistent_store = "persitence.dat"
   begin
     File.open($persistent_store) do |f|
       $stringvariable1 = Marshal.load(f)
       $arrayvariable2 = Marshal.load(f)
     end
   rescue
     puts "Can not open #{$persistent_store}"
     # Initialisation if this script runs the first time
     $stringvariable1 = ""
     $arrayvariable2 = []
   end
}

END {
   File.open($persistent_store, "w+") do |f|
     Marshal.dump($stringvariable1, f)
     Marshal.dump($arrayvariable2, f)
   end
}

# simple test program
puts $stringvariable1
puts $arrayvariable2
$stringvariable1 = "Hello World"
$arrayvariable2.push(5)
puts $stringvariable1
puts $arrayvariable2


# @@PLEAC@@_14.10
#!/usr/bin/ruby -w
# Ruby has a dbi module with an architecture similar
# to the Perl dbi module: the dbi module provides an unified
# interface and uses specialized drivers for each dbms vendor
#
begin
    DBI.connect("DBI:driver:driverspecific", "username", "auth") {
        |dbh|

        dbh.do(SQL1)

        dbh.prepare(SQL2){ |sth|
            sth.execute
            sth.fetch {|row|
                # ...
            }
        } # end of block finishes the statement handle
    } # end of block closes the database connection
rescue DBI::DatabaseError => e
    puts "dbi error occurred"
    puts "Error code: #{e.err}"
    puts "Error message: #{e.errstr}"
end

#!/usr/bin/ruby -w
# dbusers - example for mysql which creates a table,
# fills it with values, retrieves the values back,
# and finally destroys the table.

require "dbi"

# replacement for the User::pwnt module
def getpwent
    result = []
    File.open("/etc/passwd") {|file|
        file.each_line {|line|
            next if line.match(/^#/)
            cols = line.split(":")
            result.push([cols[2], cols[0]])
        }
    }
    result
end

begin
    DBI.connect("DBI:Mysql:pleacdatabase", "pleac", "pleacpassword") {
        |conn|

        conn.do("CREATE TABLE users (uid INT, login CHAR(8))")

        users = getpwent

        conn.prepare("INSERT INTO users VALUES (?,?)") {|sth|
            users.each {|entry|
                sth.execute(entry[0], entry[1])
            }
        }

        conn.execute("SELECT uid, login FROM users WHERE uid < 50") {|sth|
            sth.fetch {|row|
                puts row.collect {|col|
                    if col.nil?
                        "(null)"
                    else
                        col
                    end
                }.join(", ")
            }
        }

        conn.do("DROP TABLE users")
    }
rescue DBI::DatabaseError => e
    puts "dbi error occurred"
    puts "Error code: #{e.err}"
    puts "Error message: #{e.errstr}"
end


# @@PLEAC@@_15.1
# This test program demonstrates parsing program arguments.
# It uses the optparse library, which is included with ruby 1.8
# It handles classic unix style and gnu style options
require 'optparse'

@debugmode = false
@verbose = false

ARGV.options do |opts|
    opts.banner = "Usage: ruby #{$0} [OPTIONS] INPUTFILES"

    opts.on("-h", "--help", "show this message") {
        puts opts
        exit
    }
    # The OptionParser#on method is called with a specification of short
    # options, of long options, a data type spezification and user help
    # messages for this option.
    # The method analyses the given parameter and decides what it is,
    # so you can leave out the long option if you don't need it
    opts.on("-v", "--[no-]verbose=[FLAG]", TrueClass, "run verbosly") {
        |@verbose|   # sets @verbose to true or false
    }
    opts.on("-D", "--DEBUG", TrueClass, "turns on debug mode" ){
        |@debugmode|   # sets @debugmode to true
    }
    opts.on("-c", "--count=NUMBER", Integer, "how many times we do it" ){
        |@count|      # sets @count to given integer
    }
    opts.on("-o", "--output=FILE", String, "file to write output to"){
        |@outputfile|   # sets @outputfile to given string
    }
    opts.parse!
end

# example to use the options in the main program
puts "Verbose is on" if @verbose
puts "Debugmode is on" if @debugmode
puts "Outfile is #{@outputfile}" if defined? @outputfile
puts "Count is #{@count}" if defined? @count
ARGV.each { |param|
    puts "Got parameter #{param}"
}


# @@PLEAC@@_15.4
buf = "\0" * 8
$stdout.ioctl(0x5413, buf)
ws_row, ws_col, ws_xpixel, ws_ypixel = buf.unpack("S4")

raise "You must have at least 20 characters" unless ws_col >= 20
max = 0
values = (1..5).collect { rand(20) }  # generate an array[5] of rand values
for i in values
    max = i if max < i
end
ratio = Float(ws_col-12)/max          # chars per unit
for i in values
    printf "%8.1f %s\n", i, "*" * (ratio*i)
end

# gives, for example:
#   15.0 *******************************
#   10.0 *********************
#    5.0 **********
#   14.0 *****************************
#   18.0 **************************************


# @@PLEAC@@_16.1
output = `program args`       # collect output into one multiline string
output = `program args`.split # collect output into array, one line per
element

readme = IO.popen("ls")
output = ""
while readme.gets do
    output += $_
end
readme.close

`fsck -y /dev/rsd1a`  # BAD AND SCARY in Perl because it's managed by the shell
                      # I donna in Ruby ...

# so the "clean and secure" version
readme, writeme = IO.pipe
pid = fork {
    # child
    $stdout = writeme
    readme.close
    exec('find', '..')
}
# parent
Process.waitpid(pid, 0)
writeme.close
while readme.gets do
    # do something with $_
end


# @@PLEAC@@_16.2
status = system("xemacs #{myfile}")

status = system("xemacs", myfile)

system("cmd1 args | cmd2 | cmd3 >outfile")
system("cmd args <infile >outfile 2>errfile")

# stop if the command fails
raise "$program exited funny: #{$?}" unless system("cmd", "args1", "args2")

# get the value of the signal sent to the child
# even if it is a SIGINT or SIGQUIT
system(arglist)
raise "program killed by signal #{$?}" if ($? & 127) != 0

pid = fork {
    trap("SIGINT", "IGNORE")
    exec("sleep", "10")
}
trap ("SIGINT") {
    puts "Tsk tsk, no process interruptus"
}
Process.waitpid(pid, 0)

# Ruby doesn't permit to lie to the program called by a 'system'.
# (ie specify what return argv[0] in C, $0 in Perl/Ruby ...)
# A (dirty) way is to create a link (under Unix), run this link and
# erase it. Somebody has a best idea ?


# @@PLEAC@@_16.3
exec("archive *.data")

exec("archive", "accounting.data")

exec("archive accounting.data")


# @@PLEAC@@_16.4
# read the output of a program
IO.popen("ls") {|readme|
    while readme.gets do
        # ...
    end
}
# or
readme = IO.popen("ls")
while readme.gets do
    # ...
end
readme.close

# "write" in a program
IO.popen("cmd args","w") {|pipe|
    pipe.puts("data")
    pipe.puts("foo")
}

# close wait for the end of the process
read = IO.popen("sleep 10000") # child goes to sleep
read.close                     # and the parent goes to lala land

writeme = IO.popen("cmd args", "w")
writeme.puts "hello" # program will get hello\n on STDIN
writeme.close        # program will get EOF on STDIN

# send in a pager (eg less) all output
$stdout = IO.popen("/usr/bin/less","w")
print "huge string\n" * 10000


# @@PLEAC@@_16.5
#-----------------------------
def head(lines = 20)
    pid = open("|-","w")
    if pid == nil
        return
    else
        while gets() do
            pid.print
            lines -= 1
            break if lines == 0
        end
    end
    exit
end

head(100)
while gets() do
    print
end
#-----------------------------
1: > Welcome to Linux, version 2.0.33 on a i686

2: > 

3: >     "The software required `Windows 95 or better', 

4: >      so I installed Linux."  
#-----------------------------
> 1: Welcome to Linux, Kernel version 2.0.33 on a i686

> 2: 

> 3:     "The software required `Windows 95 or better', 

> 4:      so I installed Linux."  
#-----------------------------
#!/usr/bin/ruby
# qnumcat - demo additive output filters

def number()
    pid = open("|-","w")
    if pid == nil
        return
    else
        while gets() do pid.printf("%d: %s", $., $_); end
    end
    exit
end

def quote()
    pid = open("|-","w")
    if pid == nil
        return
    else
        while gets() do pid.print "> #{$_}" end
    end
    exit
end

number()
quote()

while gets() do
    print
end
$stdout.close
exit


# @@PLEAC@@_16.6
ARGV.map! { |arg|
    arg =~ /\.(gz|Z)$/ ? "|gzip -dc #{arg}" : arg
}
for file in ARGV
    fh = open(file)
    while fh.gets() do
        # .......
    end
end
#-----------------------------
ARGV.map! { |arg|
    arg =~ %r#^\w+://# ? "|GET #{arg}" : arg   #
}
for file in ARGV
    fh = open(file)
    while fh.gets() do
        # .......
    end
end
#-----------------------------
pwdinfo = (`domainname` =~ /^(\(none\))?$/) ? '/etc/passwd' : '|ypcat  passwd';
pwd = open(pwdinfo);
#-----------------------------
puts "File, please? ";
file = gets().chomp();
fh = open(file);


# @@PLEAC@@_16.7
output = `cmd 2>&1`                            # with backticks
# or
ph = open("|cmd 2>&1")                         # with an open pipe
while ph.gets() { }                            # plus a read
#-----------------------------
output = `cmd 2>/dev/null`                     # with backticks
# or
ph = open("|cmd 2>/dev/null")                  # with an open pipe
while ph.gets() { }                            # plus a read
#-----------------------------
output = `cmd 2>&1 1>/dev/null`                # with backticks
# or
ph = open("|cmd 2>&1 1>/dev/null")             # with an open pipe
while ph.gets() { }                            # plus a read
#-----------------------------
output = `cmd 3>&1 1>&2 2>&3 3>&-`             # with backticks
# or
ph = open("|cmd 3>&1 1>&2 2>&3 3>&-")          # with an open pipe
while ph.gets() { }                            # plus a read
#-----------------------------
system("program args 1>/tmp/program.stdout 2>/tmp/program.stderr") 
#-----------------------------
output = `cmd 3>&1 1>&2 2>&3 3>&-`  
#-----------------------------
fd3 = fd1 
fd1 = fd2 
fd2 = fd3 
fd3 = nil 
#-----------------------------
system("prog args 1>tmpfile 2>&1") 
system("prog args 2>&1 1>tmpfile") 
#-----------------------------
# system ("prog args 1>tmpfile 2>&1") 
fd1 = "tmpfile"          # change stdout destination first
fd2 = fd1                # now point stderr there, too
#-----------------------------
# system("prog args 2>&1 1>tmpfile") 
fd2 = fd1                # stderr same destination as stdout
fd1 = "tmpfile"          # but change stdout destination 
#-----------------------------
# It is often better not to rely on the shell, 
# because of portability, possible security problems 
# and bigger resource usage. So, it is often better to use the open3 library. 
# See below for an example.
# opening stdin, stdout, stderr
require "open3"
stdin, stdout, stderr = Open3.popen('cmd')


# @@PLEAC@@_16.8
#-----------------------------
# Contrary to perl, we don't need to use a module in Ruby
fh = Kernel.open("|" + program, "w+")
fh.puts "here's your input\n"
output = fh.gets()
fh.close()
#-----------------------------
Kernel.open("|program"),"w+")    # RIGHT !
#-----------------------------
# Ruby has already object methods for I/O handles
#-----------------------------
begin
    fh = Kernel.open("|" + program_and_options, "w+")
rescue
    if ($@ ~= /^open/)
        $stderr.puts "open failed : #{$!} \n #{$@} \n"
        break
    end
    raise      # reraise unforseen exception
end


# @@PLEAC@@_16.13
#% kill -l
#HUP INT QUIT ILL TRAP ABRT BUS FPE KILL USR1 SEGV USR2 PIPE
#ALRM TERM CHLD CONT STOP TSTP TTIN TTOU URG XCPU XFSZ VTALRM
#PROF WINCH POLL PWR
#-----------------------------
#% ruby -e 'puts Signal.list.keys.join(" ")'
#PWR USR1 BUS USR2 TERM SEGV KILL POLL STOP SYS TRAP IOT HUP INT                                                                          #
#WINCH XCPU TTIN CLD TSTP FPE IO TTOU PROF CHLD CONT PIPE ABRT
#VTALRM QUIT ILL XFSZ URG ALRM
#-----------------------------
# After that, the perl script create an hash equivalent to Signal.list, 
# and an array. The array can be obtained by :
signame = []
Signal.list.each { |name, i| signame[i] = name }


# @@PLEAC@@_16.14
Process.kill(9, pid)                    # send $pid a signal 9
Process.kill(-1, Process.getpgrp())     # send whole job a signal 1
Process.kill("USR1", $$)                # send myself a SIGUSR1
Process.kill("HUP", pid1, pid2, pid3)   # send a SIGHUP to processes in @pids
#-----------------------------
begin
    Process.kill(0, minion)
    puts "#{minion} is alive!"
rescue Errno::EPERM                     # changed uid
    puts "#{minion} has escaped my control!";
rescue Errno::ESRCH
    puts "#{minion} is deceased.";      # or zombied
rescue
    puts "Odd; I couldn't check the status of #{minion} : #{$!}"
end


# @@PLEAC@@_16.15
Kernel.trap("QUIT", got_sig_quit)       # got_sig_quit = Proc.new { puts "Quit\n" }
trap("PIPE", "got_sig_quit")            # def got_sig_pipe ...
trap("INT") { ouch++ }                  # increment ouch for every SIGINT
#-----------------------------
trap("INT", "IGNORE")                   # ignore the signal INT
#-----------------------------
trap("STOP", "DEFAULT")                 # restore default STOP signal handling


# @@PLEAC@@_16.16
# the signal handler
def ding
    trap("INT", "ding")
    puts "\aEnter your name!"
end

# prompt for name, overriding SIGINT
def get_name
    save = trap("INT", "ding")

    puts "Kindly Stranger, please enter your name: "
    name = gets().chomp()
    trap("INT", save)
    name
end


# @@PLEAC@@_16.21
# implemented thanks to http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-talk/1760
require 'timeout'

# we'll do something vastly more useful than cookbook to demonstrate timeouts
begin
    timeout(5) {
        waitsec = rand(10)
        puts "Let's see if a sleep of #{waitsec} seconds is longer than 5 seconds..."
        system("sleep #{waitsec}")
    }
    puts "Timeout didn't occur"
rescue Timeout::Error    
    puts "Timed out!"
end


# @@PLEAC@@_17.1
# A basic TCP client connection
require 'socket'
begin
    t = TCPSocket.new('www.ruby-lang.org', 'www')
rescue
    puts "error: #{$!}"
else
    # ... do something with the socket
    t.print "GET / HTTP/1.0\n\n"
    answer = t.gets(nil)
    # and terminate the connection when we're done
    t.close
end

# Using the evil low level socket API
require 'socket'
# create a socket
s = Socket.new(Socket::AF_INET, Socket::SOCK_STREAM, 0)
# build the address of the remote machine
sockaddr_server = [Socket::AF_INET, 80,
    Socket.gethostbyname('www.ruby-lang.org')[3],
    0, 0].pack("snA4NN")
# connect
begin
    s.connect(sockaddr_server)
rescue
    puts "error: #{$!}"
else
    # ... do something with the socket
    s.print "GET / HTTP/1.0\n\n"
    # and terminate the connection when we're done
    s.close
end

# TCP connection with management of error (DNS)
require 'socket'
begin
    client = TCPSocket.new('does not exists', 'www')
rescue
    puts "error: #{$!}"
end

# TCP connection with a time out
require 'socket'
require 'timeout'
begin
    timeout(1) do #the server has one second to answer
        client = TCPSocket.new('www.host.com', 'www')
    end
rescue
    puts "error: #{$!}"
end


# @@PLEAC@@_17.12
require 'socket'

class Preforker 
    attr_reader (:child_count)
    
    def initialize(prefork, max_clients_per_child, port, client_handler)
        @prefork = prefork
        @max_clients_per_child = max_clients_per_child
        @port = port
        @child_count = 0
        
        @reaper = proc {
            trap('CHLD', @reaper)
            pid = Process.wait
            @child_count -= 1
        }
        
        @huntsman = proc {
            trap('CHLD', 'IGNORE')
            trap('INT', 'IGNORE')
            Process.kill('INT', 0)
            exit
        }
        
        @client_handler=client_handler
    end
    
    def child_handler
        trap('INT', 'EXIT')
        @client_handler.setUp
        # wish: sigprocmask UNblock SIGINT
        @max_clients_per_child.times {
            client = @server.accept or break
            @client_handler.handle_request(client)
            client.close
        }
        @client_handler.tearDown
    end
    
    def make_new_child
        # wish: sigprocmask block SIGINT
        @child_count += 1
        pid = fork do
            child_handler
        end
        # wish: sigprocmask UNblock SIGINT
    end
    
    def run
        @server = TCPserver.open(@port)
        trap('CHLD', @reaper)
        trap('INT', @huntsman)
        loop {
            (@prefork - @child_count).times { |i|
                make_new_child
            }
            sleep .1
        }
    end
end

#-----------------------------
#!/usr/bin/ruby

require 'Preforker'

class ClientHandler
    def setUp
    end
    
    def tearDown
    end
    
    def handle_request(client)
        # do stuff
    end
end

server = Preforker.new(1, 100, 3102, ClientHandler.new)
server.run


# @@PLEAC@@_18.2
require 'net/ftp'

begin
    ftp = Net::FTP::new("ftp.host.com")
    ftp.login(username,password)
    ftp.chdir(directory)
    ftp.get(filename)
    ftp.put(filename)
rescue Net::FTPError
    $stderr.print "FTP failed: " + $!
ensure
    ftp.close() if ftp
end

# A better solution for a local use could be :
Net::FTP::new("ftp.host.com") do |ftp|
    ftp.login(username,password)
    ftp.chdir(directory)
    ftp.get(filename)
    ftp.put(filename)
end

# If you have only one file to get, there is a simple solution :
require 'open-uri'
open("ftp://www.ruby-lang.org/path/filename") do |fh|
    # read from filehandle fh
end 
#--------------------------------------------
# to wait a defined time for the connection, 
# use the timeout module
require 'timeout'
begin 
    timeout(30){
        ftp = Net::FTP::new("ftp.host.com")
        ftp.debug_mode = true
    }
rescue Net::FTPError
    $stderr.puts "Couldn't connect."
rescue Timeout::Error
    $stderr.puts "Timeout while connecting to server."
end

begin
    ftp.login()
rescue Net::FTPError
    $stderr.print "Couldn't authentificate.\n"
end

begin
    ftp.login(username)
rescue Net::FTPError
    $stderr.print "Still couldn't authenticate.\n"
end

begin
    ftp.login(username, password)
rescue Net::FTPError
    $stderr.print "Couldn't authenticate, even with explicit
    username and password.\n"
end

begin
    ftp.login(username, password, account)
rescue Net::FTPError
    $stderr.print "No dice. It hates me.\n"
end
#-----------------------------
ftp.put(localfile, remotefile)
#-----------------------------
# Sending data from STDIN is not directly supported 
# by the ftp library module. A possible way to do it is to use the 
# storlines method directly to send raw commands to the ftp server.
#-----------------------------
ftp.get(remotefile, localfile)
#-----------------------------
ftp.get(remotefile) { |data| puts data }
#-----------------------------
ftp.chdir("/pub/ruby") 
print "I'm in the directory ", ftp.pwd(), "\n"
#-----------------------------
ftp.mkdir("/pub/ruby/new_dir")
#-----------------------------
lines = ftp.ls("/pub/ruby/")
# => ["drwxr-xr-x 2 matz users 4096 July 17 1998 1.0", ... ]

latest = ftp.dir("/pub/ruby/*.tgz").sort.last

ftp.nlst("/pub/ruby")
# => ["/pub/ruby/1.0", ... ]
#-----------------------------
ftp.quit()


# @@PLEAC@@_18.6
require 'net/telnet'
t = Net::Telnet::new( "Timeout" => 10,
                      "Prompt"  => /%/,
                      "Host"    => host )
t.login(username, password)
files = t.cmd("ls")
t.print("top")
process_string = t.waitfor(/\d+ processes/)
t.close
#-----------------------------
/[$%#>] \z/n
#-----------------------------
# In case of an error, the telnet module throws an exception.
# For control of the behavior in case of an error,
# you just need to catch the exceptions and do your custom
# error handling.
#-----------------------------
begin
    telnet.login(username, password)
rescue TimeoutError
    fail "Login failed !\n"
end
#-----------------------------
telnet.waitfor('/--more--/')
#-----------------------------
telnet.waitfor(String => 'greasy smoke', Timeout => 30)


# @@PLEAC@@_18.7
require 'ping'

puts "#{host} is alive.\n" if Ping.pingecho(host);
#-----------------------------
# the ping module only use TCP ping, not ICMP even if we are root
if Ping.pingecho("kingkong.com")
    puts "The giant ape lives!\n";
else
    puts "All hail mighty Gamera, friend of children!\n";
end


# @@PLEAC@@_19.0
#-----------------------------
# http://www.perl.com/CPAN/
# http://www.perl.com:8001/bad/mojo.html
# ftp://gatekeeper.dec.com/pub/misc/netlib.tar.Z
# ftp://anonymous@myplace:gatekeeper.dec.com/pub/misc/netlib.tar.Z
# file:///etc/motd
#-----------------------------
# http://mox.perl.com/cgi-bin/program?name=Johann&born=1685
#-----------------------------
# http://mox.perl.com/cgi-bin/program
#-----------------------------


# @@PLEAC@@_19.1
#!/usr/local/bin/ruby -w
# hiweb - load CGI class to decode information given by web server

require 'cgi'

cgi = CGI.new('html3')

# get a parameter from a form
value = cgi.params['PARAM_NAME'][0]

# output a document
cgi.out {
    cgi.html {
        cgi.head { cgi.title { "Howdy there!" } } +
            cgi.body { cgi.p { "You typed: " + cgi.tt {
                    CGI.escapeHTML(value) } } }
    }
}

require 'cgi'
cgi = CGI.new
who   = cgi.param["Name"][0]     # first param in list
phone = cgi.param["Number"][0]
picks = cgi.param["Choices"]     # complete list

print cgi.header( 'type' => 'text/plain',
                  'expires' => Time.now + (3 * 24 * 60 * 60) )


# @@PLEAC@@_19.3
#!/usr/local/bin/ruby -w
# webwhoami - show web user's id
require 'etc'
print "Content-Type: text/plain\n\n"
print "Running as " + Etc.getpwuid.name + "\n"

# % ruby -wc cgi-script     # just check syntax

# % ruby -w  cgi-script     # params from stdin
# (offline mode: enter name=value pairs on standard input)
# name=joe
# number=10
# ^D

# % ruby -w  cgi-script name=joe number=10     # run with mock form input
# % ruby -d  cgi-script name=joe number=10     # ditto, under the debugger

# POST method script in csh
# % (setenv HTTP_METHOD POST; ruby -w cgi-script name=joe number=10)
# POST method script in sh
# % HTTP_METHOD=POST perl -w cgi-script name=joe number=10


# @@PLEAC@@_19.4
# ruby has several security levels, the level "1" is similar to perls taint mode.
# It can be switched on by providing the -T command line parameter
# or by setting $SAFE to 1. Setting $SAFE to 2,3 or 4 restricts possible
# harmful operations further.

#!/usr/bin/ruby -T
$SAFE = 1
File.open(ARGV[0], "w")
# ruby warns with:
# taint1.rb:2:in `initialize': Insecure operation - initialize (SecurityError)

$SAFE = 1
file = ARGV[0]
unless /^([\w.-]+)$/.match(file)
    raise "filename #{file} has invalid characters"
end
file = $1
# In ruby, even the back reference from a regular expression stays tainted.
# you need to explicitly untaint the variable:
file.untaint
File.open(file, "w")

# Race condition exists like in perl:
unless File.exists(filename)        # Wrong because of race condition
    File.open(filename, "w")
end


# @@PLEAC@@_19.8
url = "http://pleac.sourceforge.net/pleac_ruby/"
print "Location: #{url}\r\n\r\n"
exit

#!/usr/bin/ruby
require 'cgi'

cgi = CGI.new
oreo = CGI::Cookie.new('name' => 'filling',
                       'value' => 'vanilla creme',
                       'expires' => Time.now + (3 * 30 * 24 * 60 * 60),
                       'domain' => '.pleac.sourceforge.net')

whither = 'http://pleac.sourceforge.net/pleac_ruby/cgiprogramming.html'

cgi.out('cookie' => oreo,
        'Location' => whither){""}

#!/usr/bin/ruby
# os_snipe - redirect to a Jargon File entry about current OS
dir = 'http://www.elsewhere.org/jargon/html/entry'

agent = ENV['HTTP_USER_AGENT']

page = case
    when agent =~ /Mac/: 'Macintrash.html'
    when agent =~ /Win(dows )?NT/: 'evil_and_rude.html'
    when agent =~ /Win|MSIE|WebTV/: 'Microsloth_Windows.html'
    when agent =~ /Linux/: 'Linux.html'
    when agent =~ /HP-UX/: 'HP-SUX.html'
    when agent =~ /SunOS/: 'ScumOS.html'
    else 'Appendix_B.html'
end

print "Location: #{dir}/#{page}\n\n"

require 'cgi'
cgi = CGI.new
cgi.out('status' => '204 No response'){""}
# this produces:
# Status: 204 No response
# Content-Type: text/html
# Content-Length: 0
# <blank line here>


# @@PLEAC@@_19.10
preference_value = cgi.cookies["preference name"][0]

packed_cookie = CGI::Cookie.new("name" => "preference name",
                                "value" => "whatever you'd like",
                                "expires" => Time.local(Time.now.year + 2,
    Time.now.mon, Time.now.day, Time.now.hour, Time.now.min, Time.now.sec) )

cgi.header("cookie" => [packed_cookie])

#!/usr/local/bin/ruby -w
# ic_cookies - sample CGI script that uses a cookie
require 'cgi'

cgi = CGI.new('html3')

cookname = "favorite ice cream"
favorite = cgi.params["flavor"][0]
tasty    = cgi.cookies[cookname][0] || 'mint'

unless favorite
    cgi.out {
        cgi.html {
            cgi.head { cgi.title { "Ice Cookies" } } +
            cgi.body {
                cgi.h1 { "Hello Ice Cream" } +
                cgi.hr +
                cgi.form {
                    cgi.p { "Please select a flavor: " +
                            cgi.text_field("flavor", tasty ) }
                } +
                cgi.hr
            }
        }
    }
else
    cookie = CGI::Cookie.new( "name"    => cookname,
                              "value"   => favorite,
                              "expires" => Time.local(Time.now.year + 2,
Time.now.mon, Time.now.day, Time.now.hour, Time.now.min, Time.now.sec) )
    cgi.out("cookie" => [cookie]) {
        cgi.html {
            cgi.head { cgi.title { "Ice Cookies" } } +
            cgi.body {
                cgi.h1 { "Hello Ice Cream" } +
                cgi.p { "You chose as your favorite flavor `#{favorite}'." }
            }
        }
    }
end


# @@PLEAC@@_20.9
def templatefile(filename, fillings)
    aFile = File.new(filename, "r")
    text = aFile.read()
    aFile.close()
    pattern = Regexp.new('%%(.*?)%%')
    text.gsub!(pattern) {
        fillings[$1] || ""
    }
    text
end

fields = {
    'username' => whats_his_name,
    'count' => login_count,
    'total' => minutes_used
}
puts templatefile('simple.template', fields)

# @@INCOMPLETE@@
# An example using databases is missing


