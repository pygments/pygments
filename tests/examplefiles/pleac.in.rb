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

