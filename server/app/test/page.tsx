import Dropdown from "@/components/dropdown/dropdown";
import Sudoku from "@/components/sudoku/sudoku";

export default function Home() {
	return (
		<body>
			<header>Puzzle Solver</header>
			<main className="container-fluid grid">
				<article>
					<header>Difficulty</header>
					<Dropdown difficulty={"Beginner"} />
					<Dropdown difficulty={"Easy"} />
					<Dropdown difficulty={"Intermediate"} />
					<Dropdown difficulty={"Tricky"} />
					<Dropdown difficulty={"Fiendish"} />
				</article>

				<article>
					<header>Sudoku</header>
					<Sudoku />
				</article>
			</main>
			<footer>Created by Group 30 of DATX11</footer>
		</body>
	);
}
