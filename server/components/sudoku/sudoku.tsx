import React from "react";
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

	console.log(sudoku);

	const blocks: string[] = Array(9).fill("");
	for (let i = 0; i < sudoku.length; i++) {
		const block = Math.floor(i / 27) * 3 + Math.floor((i % 9) / 3);
		blocks[block] = blocks[block].concat(sudoku[i]);
	}

	return (
		<div className="sudoku overflow-auto">
			{[...Array(9)].map((_, i) => {
				return <Block key={i} values={blocks[i].split("")} />;
			})}
		</div>
	);
}
