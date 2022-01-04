match command.split():
    case ['to', direction] if direction in destination.route:
        return 1
    case 'foo' | 'bar':
        return 2
    case 'raz' as name:
        return 3
    case ['to', _]:
        return 4
    case else_bar:
        return 5
    case _:
        return 6

match command.split():
    case _Direction():
        return 1
    case _ as default:
        return 2

case = 1
match = 1
match if True else bar
