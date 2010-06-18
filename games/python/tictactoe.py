import re
import itertools as it

import cStringIO

BLANK = 0x0
NOUGHT = 0x1
CROSS = 0x2

VALID = set((NOUGHT, CROSS))

SYMBOLS = {
    BLANK: '_',
    NOUGHT: 'O',
    CROSS: 'X'
}

def _pair_to_index(x_pos, y_pos):
    return x_pos + 3 * y_pos

def _index_to_pair(pos):
    pair = divmod(pos, 3)
    return pair[1], pair[0]

def sure_win(three):
    return len(set(three)) == 1

def close_win(three):
    blanks = three.count(BLANK)
    if blanks > 1:
        return False
    elif blank == 1 and three.count(NOUGHT) == 2:
        return True
    elif blank == 1 and three.count(CROSS) == 2:
        return True
    else:
        return False



class Board(object):
    def __init__(self):
        self._board = [BLANK, ] * 9

    def __getitem__(self, pos):
        try:
            x_pos, y_pos = pos
        except ValueError:
            return self._board[pos]
        else:
            return self._board[_pair_to_index(x_pos, y_pos)]

    def __setitem__(self, pos, val):
        if val in VALID:
            try:
                x_pos, y_pos = pos
            except ValueError: pass
            else:
                pos = _pair_to_index(x_pos, y_pos)
            if self._board[pos] in VALID:
                raise ValueError('Already set position')
            else:
                self._board[pos] = val
        else:
            raise ValueError('Not a valid move')

    def col(self, index):
        return self._board[index::3]

    def row(self, index):
        return self._board[3*index:3*index+3]

    def cols(self):
        return it.imap(self.col, range(3))

    def rows(self):
        return it.imap(self.row, range(3))

    def diag(self):
        return [self._board[0], self._board[4], self._board[8]]

    def antidiag(self):
        return [self._board[6], self._board[4], self._board[2]]

    def threes(self):
        return it.chain(self.rows(), self.cols(),
                        (self.diag(), self.antidiag()))

    def winner(self):
        for three in self.threes():
            if sure_win(three):
                return three.pop()
        else:
            return None

    def draw(self):
        '''Nobody can win'''
        # try to discover draws asap
        return BLANK not in self._board

    def __str__(self):
        sio = cStringIO.StringIO()
        for row in self.rows():
            print >>sio, SYMBOLS[row[0]], SYMBOLS[row[1]], SYMBOLS[row[2]]
        sio.seek(0)
        return sio.read()


class Player(object):
    def __init__(self, whoami, strategy):
        assert whoami in VALID
        self.board = Board()
        self.strategy = strategy
        self.whoami = whoami

    def notify_move(self, move):
        pos, value = move
        self.board[pos] = value

    def next_move(self):
        return self.strategy.next_move(self.whoami, self.board)

class AskTheHuman(object):
    rex = re.compile('(\d+),\s+(\d+)')

    def tentative_move(self, tentative, whoami, board):
        line = raw_input('P%s: %s> ' % (whoami, tentative)).strip()
        match = self.rex.match(line)
        if match:
            return tuple(int(g) for g in match.groups())
        else:
            try:
                return _index_to_pair(int(line))
            except ValueError:
                pass

    def next_move(self, whoami, board):
        for tentative in it.count(1):
            move = self.tentative_move(tentative, whoami, board)
            if board[move] == BLANK:
                return move

class Game(object):
    def __init__(self, *args):
        self.board = Board()
        self.players = args
        self.moves = 0

    def next_move(self):
        player = self.players[self.moves % 2]
        move = player.next_move()
        return (move, player.whoami)

    def perform_move(self, move):
        self.board.__setitem__(*move)
        self.moves += 1

    def notify_move(self, move):
        for p in self.players:
            p.notify_move(move)

    def play(self):
        while 1:
            print self.board
            if self.board.draw():
                print 'DRAW!'
                break
            possible_winner = self.board.winner()
            if possible_winner:
                print 'Player %s won!' % possible_winner
                break
            for i in range(3):
                move = self.next_move()
                try:
                    self.perform_move(move)
                except ValueError:
                    print 'Invalid move!'
                else:
                    self.notify_move(move)
                    break
            else:
                print 'TOO MANY INVALID MOVES'
                break

if __name__ == '__main__':
    game = Game(Player(NOUGHT, AskTheHuman()),
                Player(CROSS, AskTheHuman()))
    game.play()

