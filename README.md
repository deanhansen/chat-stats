# Visulize your ChatGPT conversation history!

## ✨ Features
- Personalized Insights: Discover your usage habits with key statistics, including the total number of conversations, messages sent, and your most active days.
- Interactive Visualizations: Dive deeper into your data with interactive plots built using the ggiraph package. Hover over data points to reveal details and save plots to share with others.
- Token Analysis: Understand the "length" of your conversations by visualizing the token distribution for both your messages and ChatGPT's responses.
- Aesthetic and Minimalist Design: The app features a custom "hacker" theme with a dark background and neon green text, giving it a unique and modern feel.
- Pre-Dashboard Survey: Before you view your data, a fun, auto-generated quiz tests your knowledge of your own ChatGPT usage and general AI facts.
- Data Privacy: All data processing is done locally within the app; your conversation history is never stored or sent to a remote server.

## 🚀 How to Use

- Download Your Data: First, you need to export your conversation history from OpenAI. Follow the official instructions on the OpenAI website to download your data. This will provide you with a zip file containing conversations.json.
- Launch the App: Run the app locally on your machine.
- Upload File: On the welcome screen, click the "Upload File" button and select the conversations.json file from your downloaded zip folder.
- Take the Survey: Once the data is processed, a survey will pop up. Answer the questions to see how well you know your own habits.
- Explore Your Dashboard: After the survey, the full dashboard will be revealed, allowing you to explore your ChatGPT usage in detail.

## 📊 Dashboard Visuals

- Key Metrics Boxes: A row of boxes at the top displays quick stats like total conversations, total messages, average token length, and your most frequent day for chatting.
- Conversations by Weekday: A bar chart shows a breakdown of how many conversations you start on each day of the week.
- Token Distribution (ECDF Plot): This plot compares the cumulative distribution of token counts for both your messages and ChatGPT's responses, providing a clear picture of who is more verbose.
- Most Common Conversation Topics: A bar chart (or optional word cloud) reveals the top words from your conversation titles, giving you a quick summary of the topics you discuss most often.
- Raw Data Table: A searchable and sortable table of your entire conversation history, with an option to download the data as a CSV.
