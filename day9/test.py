
dirs = {
    'U': (0, 1),
    'D': (0, -1),
    'L': (-1, 0),
    'R': (1, 0),
}


def L0_distance(p1, p2):
    return max(abs(p1[0] - p2[0]), abs(p1[1] - p2[1]))


def read_input(filename='input.txt'):
    with open(filename) as f:
        # each line contains direction and number of steps, for ex. U 6
        return [(dirs[line.split()[0]], int(line.split()[1])) for line in f.readlines()]


def execute_command(command, body_pos, tail_visited_map):
    direction, steps = command
    for _ in range(steps):
        body_pos[0] = (body_pos[0][0] + direction[0],
                       body_pos[0][1] + direction[1])

        # adjust body parts to follow head
        for i in range(1, len(body_pos)):
            if L0_distance(body_pos[i], body_pos[i - 1]) > 1:
                new_x, new_y = body_pos[i]
                if body_pos[i][0] != body_pos[i - 1][0]:
                    # move in x direction towards previous body part.
                    new_x += (body_pos[i - 1][0] - body_pos[i][0]) / \
                        abs((body_pos[i - 1][0] - body_pos[i][0]))
                if body_pos[i][1] != body_pos[i - 1][1]:
                    # move in y direction towards previous body part.
                    new_y += (body_pos[i - 1][1] - body_pos[i][1]) / \
                        abs((body_pos[i - 1][1] - body_pos[i][1]))
                body_pos[i] = (new_x, new_y)
        tail_visited_map[body_pos[-1]] = True

    return body_pos


def solve(commands, body_length):
    body_pos = [(0, 0) for _ in range(body_length)]
    tail_visited_map = {body_pos[-1]: True}
    for command in commands:
        body_pos = execute_command(command, body_pos, tail_visited_map)
    return len(tail_visited_map)


if __name__ == "__main__":
    print("For debugging:")
    commands = read_input()
    print(f"commands[0]: {commands[0]}")
    print(f"commands[1]: {commands[1]}")
    print("...")
    print(f"commands[-1]: {commands[-1]}")
    print()

    print(f"Part 1: tail visited total {solve(commands, 2)} unique places")
    print(f"Part 2: tail visited total {solve(commands, 10)} unique places")
