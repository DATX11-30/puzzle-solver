import type { Metadata } from "next";
import { Inter } from "next/font/google";
import "./globals.css";

const inter = Inter({ subsets: ["latin"] });

export const metadata: Metadata = {
	title: {
		default: "Puzzle Solver DATX11G30",
		template: "%s | DATX11G30"
	},
	description: "Puzzle Solver DATX11 Group 30",
	generator: "Next.js",
	applicationName: "Puzzle Solver",
	referrer: "origin-when-cross-origin",
	keywords: ["Next.js", "React", "JavaScript"],
	authors: [{ name: "Max Hagman", url: "maxhagman.se" }],
	creator: "Group 30 DATX11",
	formatDetection: {
		email: true,
		address: true,
		telephone: true
	},
	icons: {
		icon: [
			{
				media: "(prefers-color-scheme: light)",
				url: "/images/icon-dark-head.svg",
				href: "/images/icon-dark-head.svg"
			},
			{
				media: "(prefers-color-scheme: dark)",
				url: "/images/icon-light-head.svg",
				href: "/images/icon-light-head.svg"
			}
		]
	}
};

export default function RootLayout({
	children
}: Readonly<{
	children: React.ReactNode;
}>) {
	return <html lang="en">{children}</html>;
}
