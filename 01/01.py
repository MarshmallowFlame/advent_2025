

def zero_counter(filename):

    with open(filename) as file:
        zero_count = 0
        position = 50
        for line in file:
            direction = 1 if line.startswith("R") else -1
            steps = int(line[1:]) * direction
            position = (position + steps) % 100
            if position == 0:
                zero_count += 1
        print(zero_count)


if __name__ == "__main__":
    zero_counter("input.txt")