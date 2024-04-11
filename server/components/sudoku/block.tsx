import Cell from "./cell";

export default function Block({ values, notes }: { values?: string; notes?: string[][] }) {
	let value: string[] = new Array(9).fill("0");
	if (values) {
		for (let i = 0; i < values.length; i++) {
			value[i] = values[i];
		}
	}

	if (notes) {
		return (
			<div className="block">
				{[...Array(9)].map((_, i) => {
					return <Cell key={i} value={value[i]} notes={notes[i]} />;
				})}
			</div>
		);
	}

	return (
		<div className="block">
			{[...Array(9)].map((_, i) => {
				return <Cell key={i} value={value[i]} />;
			})}
		</div>
	);
}
