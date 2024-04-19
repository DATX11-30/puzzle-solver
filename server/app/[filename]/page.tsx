"use client";

import Dropdown from "@/components/dropdown/dropdown";
import Sudoku from "@/components/sudoku/sudoku";
import { getSolution } from "@/lib/getSolution";
import Link from "next/link";
import { useParams, useSearchParams } from "next/navigation";
import React, { useEffect, useState } from "react";

type Props = {};

const page = (props: Props) => {
	const { filename } = useParams();
	const searchParams = useSearchParams();
	const [partialSuds, setPartialSuds] = useState<string[] | null>(null);
	const [techniques, setTechniques] = useState<string[] | null>(null);

	const [currentIndex, setCurrentIndex] = useState<number>(
		searchParams.has("step") ? parseInt(searchParams.get("step") as string) - 1 : 0
	);

	const handleKeyDown = (event: KeyboardEvent) => {
		if (!partialSuds) return;

		if (event.key == "ArrowRight") {
			setCurrentIndex((prev) => {
				if (prev + 1 < partialSuds.length) {
					return prev + 1;
				} else {
					return prev;
				}
			});
		} else if (event.key == "ArrowLeft") {
			setCurrentIndex((prev) => {
				if (prev - 1 >= 0) {
					return prev - 1;
				} else {
					return prev;
				}
			});
		}
	};

	useEffect(() => {
		getSolution(filename as string).then((solution) => {
			console.log(solution);

			setPartialSuds(solution.partialSuds);
			setTechniques(solution.techniques);
		});
	}, []);

	useEffect(() => {
		window.addEventListener("keydown", handleKeyDown);

		return () => {
			window.removeEventListener("keydown", handleKeyDown);
		};
	}, [partialSuds]);

	if (!partialSuds || !techniques) {
		return <div>Loading...</div>;
	}

	let technique = techniques[currentIndex].split("").splice(0, techniques[currentIndex].length - 1);
	let i = technique.findIndex((element) => element === "(");
	let highlight = undefined;
	if (i > -1) {
		try {
			highlight = { row: parseInt(technique[i + 3]), col: parseInt(technique[i + 1]) };
		} catch (error) {
			console.error(error);
		}
	}

	return (
		<article
			className="container-fluid"
			style={{
				height: "100%",
				display: "flex",
				flexFlow: "column nowrap",
				justifyContent: "space-between"
			}}
		>
			<header className="cursor-pointer">
				<Link href={"/"}>Puzzle Solver</Link>
			</header>
			<div
				style={{
					display: "flex",
					flexWrap: "nowrap",
					flexDirection: "row",
					justifyContent: "center",
					alignContent: "center",
					width: "100%"
				}}
			>
				<article className="container-fluid" style={{ marginRight: "1%" }}>
					<header>
						<p>
							{currentIndex + 1} / {partialSuds.length} {" " + filename}
						</p>
						<p>
							{techniques[currentIndex].split("").splice(0, techniques[currentIndex].length - 1)}
						</p>
					</header>
					<Sudoku sudoku={partialSuds[currentIndex]} highlight={highlight} />
				</article>
				<article className="container-fluid" style={{ maxWidth: "30%" }}>
					<header>Choose a difficulty</header>
					<div className="overflow-auto">
						<Dropdown difficulty={"Beginner"} />
						<Dropdown difficulty={"Easy"} />
						<Dropdown difficulty={"Medium"} />
						<Dropdown difficulty={"Tricky"} />
						<Dropdown difficulty={"Fiendish"} />
					</div>
				</article>
			</div>
			<footer>Created by Group 30 of DATX11</footer>
		</article>
	);
};

export default page;
