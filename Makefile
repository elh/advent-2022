.PHONY: run-example run target

run-example:
	@echo "Running day ${day} example..."
	@clj -M -m advent-2022.day-${day} inputs/${day}_example_input.txt

run:
	@echo "Running day ${day}..."
	@clj -M -m advent-2022.day-${day} inputs/${day}_input.txt
