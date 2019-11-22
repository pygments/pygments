# Based on examples included the RFC files.
# It doesn't show cases for every token parsed by the lexer.

require ["fileinto", "envelope"];

if address :is "to" "demo@example.org" {
  fileinto "demo-list";
} elsif envelope :is "from" "owner@example.org" {
  fileinto "lists.cipe";
} elsif (exists "X-Cron-Env",
          header :regex    ["subject"] [".* A",
                                        ".* B"]) {
  addflag "Cron";
  stop;
} else {
  keep;
}

if body :text :contains "project schedule" {
        fileinto "project/schedule";
}

require "vacation";
if header :contains "subject" "lunch" {
    vacation :handle "ran-away" "I'm out and can't meet for lunch";
} else {
    vacation :handle "ran-away" "I'm out";
}


if virustest :value "eq" :comparator "i;ascii-numeric" "0" {
  fileinto "Unscanned";
  /* Infected with high probability (value range in 1-5) */
}

if envelope :detail "to" "spam"{
  fileinto "Spam";
}

if size :over 100k {
  discard;
}
elsif address :DOMAIN :is ["From", "To"] "example.com"
{
  keep;
}

require ["reject"];

if size :over 1M {
  reject text:
Your message is too big.  If you want to send me a big attachment,
put it on a public web site and send me a URL.
.
;
}
