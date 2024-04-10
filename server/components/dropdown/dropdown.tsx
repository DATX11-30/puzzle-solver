export default function Dropdown({
	difficulty
}: {
	difficulty: "Beginner" | "Easy" | "Intermediate" | "Tricky" | "Fiendish";
}) {
	const path = "../sudoku/";
	let sudokus: { filename: string; date: string }[] = [];
	return (
		<details>
			<summary role="button">{difficulty}</summary>
			<div className="container-fluid center">
				{sudokus.map((sudoku, index) => {
					const { filename, date } = sudoku;
					return <a href={filename}>{date}</a>;
				})}
			</div>
		</details>
	);
}
