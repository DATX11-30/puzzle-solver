export default function Droplist({ items }: { items: { filename: string; date: string }[] }) {
	return (
		<div style={{ display: "flex", justifyContent: "space-around", flexDirection: "column" }}>
			{items.map((sudoku, index) => {
				const { filename, date } = sudoku;
				return (
					<a href={filename} key={index} className="contrast">
						{date}
					</a>
				);
			})}
		</div>
	);
}
