import { log } from "console";
import Block from "./block";
import "./sudoku.css";

export default function Sudoku({ sudoku }: { sudoku?: string }) {
	return (
		<div className="sudoku overflow-auto">
			{[...Array(9)].map((_, i) => {
				if (sudoku) return <Block key={i} values={sudoku.slice(i * 9, (i + 1) * 9)} />;
				return <Block key={i} />;
			})}
		</div>
	);
}

const stringToSudoku = (sudoku: string) => {
	// get the 9 first characters of sudoku
	const rows: string[] = [sudoku.slice(0, 9)];
	log(rows[0]);
	const blocks = rows.map((row) => row.match(/.{1,3}/g));
	return blocks;
};
