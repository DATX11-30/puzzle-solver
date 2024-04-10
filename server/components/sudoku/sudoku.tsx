import Block from "./block";
import "./sudoku.css";

export default function Sudoku() {
	return (
		<div className="sudoku overflow-auto">
			<Block />
			<Block />
			<Block />

			<Block />
			<Block />
			<Block />

			<Block />
			<Block />
			<Block />
		</div>
	);
}
