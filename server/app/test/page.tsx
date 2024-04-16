import Dropdown from "@/components/dropdown/dropdown";
import Sudoku from "@/components/sudoku/sudoku";

export default function Home() {
	return (
			<article
				className="container-fluid"
				style={{
					height: "100%",
					display: "flex",
					flexFlow: "column nowrap",
					justifyContent: "space-between"
				}}
			>
				<header>Puzzle solver</header>
				<div
					style={{
						display: "flex",
						flexWrap: "nowrap",
						flexDirection: "row",
						justifyContent: "center",
						alignContent: "center",
						width: "100%"
					}}
				>
					<article className="container-fluid" style={{ marginRight: "1%" }}>
						<header>Sudoku</header>
						<Sudoku />
					</article>
					<article className="container-fluid" style={{ maxWidth: "30%" }}>
						<header>Choose a difficulty</header>
						<div className="overflow-auto">
							<Dropdown difficulty={"Beginner"} />
							<Dropdown difficulty={"Easy"} />
							<Dropdown difficulty={"Medium"} />
							<Dropdown difficulty={"Tricky"} />
							<Dropdown difficulty={"Fiendish"} />
						</div>
					</article>
				</div>
				<footer>Created by Group 30 of DATX11</footer>
			</article>
	);
}
