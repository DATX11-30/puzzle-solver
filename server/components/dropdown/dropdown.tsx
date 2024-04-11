import { readdir } from "fs/promises";
import Droplist from "./droplist";

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
			<div className="center grid">
				<Droplist items={sudokus.splice(0, sudokus.length / 3)} />
				<Droplist items={sudokus.splice(0, sudokus.length / 2)} />
				<Droplist items={sudokus} />
			</div>
		</details>
	);
}
