import { readFile, readdir } from "fs/promises";

export default async function Dropdown({
	difficulty
}: {
	difficulty: "Beginner" | "Easy" | "Medium" | "Tricky" | "Fiendish";
}) {
	const path = "./sudokus/";

	const files = await readdir(path);
	let sudokus: { filename: string; date: string }[] = [];
	files.map(async (file) => {
		if (file.startsWith(difficulty)) {
			sudokus.push({
				filename: file,
				date: file.substring(difficulty.length + 1, difficulty.length + 11).replace(/_/g, "/")
			});
		}
	});
	return (
		<details>
			<summary role="button">{difficulty}</summary>
			<div style={{ display: "flex", justifyContent: "space-around", flexDirection: "column" }}>
				{sudokus.map((sudoku, index) => {
					const { filename, date } = sudoku;
					return (
						<a href={filename} key={index}>
							{date}
						</a>
					);
				})}
			</div>
		</details>
	);
}
