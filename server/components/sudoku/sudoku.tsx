import Block from "./block";
import "./sudoku.css";

export default function Sudoku({
	sudoku,
	highlight
}: {
	sudoku?: string;
	highlight?: { row: number; col: number };
}) {
	if (!sudoku) {
		return (
			<div className="sudoku overflow-auto">
				{[...Array(9)].map((_, i) => {
					return <Block key={i} />;
				})}
			</div>
		);
	}

	const sudokuValues: (string | string[])[] = [];

	const sudokuArray = sudoku?.split("");
	for (let i = 0; i < sudokuArray?.length; i++) {
		if (!isNaN(+sudokuArray[i])) {
			sudokuValues.push(sudokuArray[i]);
		} else if (sudokuArray[i] === "[") {
			sudokuValues.push([sudokuArray[i + 1], sudokuArray[i + 3]]);
			i += 3;
		}
	}

	console.log(sudokuValues);

	return (
		<div className="sudoku overflow-auto">
			{[...Array(9)].map((_, i) => {
				let highlightIndex = undefined;
				if (highlight) {
					highlightIndex =
						highlight.row === Math.floor(i / 3) && highlight.col === i % 3 ? 4 : undefined;
				}
				return (
					<Block
						key={i}
						values={sudokuValues.slice(i * 9, (i + 1) * 9)}
						highlight={highlightIndex}
					/>
				);
			})}
		</div>
	);
}
