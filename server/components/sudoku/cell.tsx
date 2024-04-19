import { log } from "console";

export default function Cell({
	notes,
	value,
	highlight
}: {
	notes?: string[];
	value?: string;
	highlight?: boolean;
}) {
	if (value && value !== "0") {
		const classes = highlight ? "cell highlight" : "cell";
		return (
			<div className={classes}>
				<div className="value" style={{ gridArea: "1 / 1 / span 3 / span 3" }}>
					{value}
				</div>
			</div>
		);
	}
	if (notes) return <div className="cell">{notes.map(generateNotes)}</div>;
	return <div className="cell"></div>;
}

const generateNotes = (note: string, index: number) => {
	return (
		<div className={toString(note) + " note"} key={index}>
			{note}
		</div>
	);
};

const toString = (note: string) => {
	switch (note) {
		case "1":
			return "one";
		case "2":
			return "two";
		case "3":
			return "three";
		case "4":
			return "four";
		case "5":
			return "five";
		case "6":
			return "six";
		case "7":
			return "seven";
		case "8":
			return "eight";
		case "9":
			return "nine";
		default:
			return "";
	}
};
