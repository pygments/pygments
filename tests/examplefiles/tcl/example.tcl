# Alt and arrow keys to scroll
set scroll_amount 2
bind Text <Alt-Up> {
    %W yview scroll -$scroll_amount units
    %W mark set insert @0,[expr [winfo height %W] / 2]
}
bind Text <Alt-Down> {
    %W yview scroll $scroll_amount units
    %W mark set insert @0,[expr [winfo height %W] / 2]
}
