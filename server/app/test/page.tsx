import Dropdown from "@/components/dropdown/dropdown";
import Sudoku from "@/components/sudoku/sudoku";

export default function Home() {
	return (
		<body
			style={{
				padding: "3%",
				// gridTemplateRows: "auto auto auto",
				alignContent: "start",
				justifyContent: "center",
				height: "100vh"
			}}
		>
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
						height: "100%",
						width: "100%"
					}}
				>
					<article className="conatiner-fluid" style={{ aspectRatio: "1/1", marginRight: "1%" }}>
						<header>Sudoku</header>
						<Sudoku />
					</article>
					<article className="container-fluid" style={{ height: "100%" }}>
						<header>Choose a difficulty</header>
						<div className="overflow-auto">
							<Dropdown difficulty={"Beginner"} />
							<Dropdown difficulty={"Easy"} />
							<Dropdown difficulty={"Intermediate"} />
							<Dropdown difficulty={"Tricky"} />
							<Dropdown difficulty={"Fiendish"} />
						</div>
					</article>
				</div>
				<footer>Created by Group 30 of DATX11</footer>
			</article>
		</body>
	);
}
