"use client";

import { getSolution } from "@/lib/getSolution";
import { useParams } from "next/navigation";
import React, { useEffect } from "react";

type Props = {};

const page = (props: Props) => {
	const { filename } = useParams();
	const [partialSuds, setPartialSuds] = React.useState<string[]>([]);
	const [techniques, setTechniques] = React.useState<string[]>([]);

	useEffect(() => {
		getSolution(filename as string).then((solution) => {
			setPartialSuds(solution.partialSuds);
			setTechniques(solution.techniques);

			console.log(solution);
		});
	}, []);

	return (
		<div>
			<h1>Techniques</h1>
			{techniques.map((technique, i) => {
				return <div key={i}>{technique}</div>;
			})}
			{partialSuds.map((partialSud, i) => {
				return <div key={i}>{partialSud}</div>;
			})}
		</div>
	);
};

export default page;
