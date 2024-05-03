import React from "react";
import Block from "./block";
import "./sudoku.css";

type Block = Array<string | string[]>;

export default function Sudoku({ sudoku }: { sudoku?: string }) {
	if (!sudoku) {
		return (
			<div className="sudoku overflow-auto">
				{[...Array(9)].map((_, i) => {
					return <Block key={i} />;
				})}
			</div>
		);
	}


	const sudokuArray = sudoku?.split("");
	const sudokuValues: Array<string | string[]> = [];

	for (let i = 0; i < sudokuArray?.length; i++) {
		if (!isNaN(+sudokuArray[i])) {
			sudokuValues.push(sudokuArray[i]);
		} else if (sudokuArray[i] === "[") {
			sudokuValues.push([sudokuArray[i + 1], sudokuArray[i + 3]]);
			i += 3;
		}
	}



	const blocks: Block[]= Array(9).fill([]);
	for (let i = 0; i < sudokuValues.length; i++) {
		const block = Math.floor(i / 27) * 3 + Math.floor((i % 9) / 3);
		blocks[block] = [...blocks[block], sudokuValues[i]];
	}



	return (
		<div className="sudoku overflow-auto">
			{[...Array(9)].map((_, i) => {
				return <Block key={i} values={blocks[i]} />;
			})}
		</div>
	);
}
