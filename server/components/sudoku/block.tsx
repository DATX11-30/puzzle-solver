import Cell from "./cell";

export default function Block() {
	return (
		<div className="block">
			<Cell notes={[1, 2, 3]} />
			<Cell notes={[5, 4, 8]} />
			<Cell notes={[2, 5, 7]} />
			<Cell />
			<Cell notes={[9]} />
			<Cell notes={[6, 7]} />
			<Cell notes={[1, 2, 3, 4, 5, 6, 7, 8, 9]} />
			<Cell notes={[9, 8, 7, 6, 5, 4, 3, 2, 1]} />
			<Cell notes={[5, 3, 7, 9]} />
		</div>
	);
}
