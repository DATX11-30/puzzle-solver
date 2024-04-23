import React from "react";
import Cell from "./cell";

export default function Block({ values }: { values?: string[] }) {
	let value: string[] = new Array(9).fill("0");

	if (!values) {
		return (
			<div className="block">
				{value.map((v, i) => {
					return <Cell key={i} value={v} />;
				})}
			</div>
		);
	}

	return (
		<div className="block">
			{values.map((v, i) => {
				if (Array.isArray(v)) {
					value = v;
					return <Cell key={i} notes={v} />;
				}
				return <Cell key={i} value={v} />;
			})}
		</div>
	);
}
