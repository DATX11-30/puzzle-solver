export default function Droplist({ items }: { items: { filename: string; date: string, hasSolution: boolean }[] }) {
	return (
		<div style={{ display: "flex", justifyContent: "space-around", flexDirection: "column" }}>
			{items.map((sudoku, index) => {
				const { filename, date, hasSolution } = sudoku;
				return (
					<a href={filename} key={index} className="contrast">
						{date} {hasSolution ? "✅": ""}
					</a>
				);
			})}
		</div>
	);
}
