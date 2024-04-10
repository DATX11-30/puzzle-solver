import { log } from "console";

export default function Cell({ notes }: { notes?: number[] }) {
	if (notes) return <div className="cell">{notes.map(generateNotes)}</div>;
	return <div className="cell"></div>;
}

const generateNotes = (note: number, index: number) => {
	return (
		<div className={toString(note)} key={index}>
			{note}
		</div>
	);
};

const toString = (note: number) => {
	switch (note) {
		case 1:
			return "one";
		case 2:
			return "two";
		case 3:
			return "three";
		case 4:
			return "four";
		case 5:
			return "five";
		case 6:
			return "six";
		case 7:
			return "seven";
		case 8:
			return "eight";
		case 9:
			return "nine";
		default:
			return "";
	}
};