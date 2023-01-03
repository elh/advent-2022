.PHONY: run-example run test lint wc

# NOTE: run-example and run are not general to all days anymore. some expect arguments

run-example:
	@echo "Running day ${day} example..."
	@clj -M -m advent-2022.day-${day} inputs/${day}_example_input.txt

run:
	@echo "Running day ${day}..."
	@clj -M -m advent-2022.day-${day} inputs/${day}_input.txt

test:
	@clj -X:test

lint:
	@clj -M:lint

wc:
	@find -s src/advent_2022 -name 'day_[[:digit:]][[:digit:]].clj' | xargs wc
