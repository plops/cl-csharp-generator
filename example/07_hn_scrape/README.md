```
@points", NpgsqlTypes.NpgsqlDbType.Integer);
                command.Parameters.Add("@user", NpgsqlTypes.NpgsqlDbType.Text);
                command.Parameters.Add("@time_ago", NpgsqlTypes.NpgsqlDbType.Text);

                foreach (var newsItem in newsItems)
                {
                    command.Parameters["@title"].Value = newsItem.Title;
                    command.Parameters["@url"].Value = newsItem.Url;
                    command.Parameters["@points"].Value = newsItem.Points;
                    command.Parameters["@user"].Value = newsItem.User;
                    command.Parameters["@time_ago"].Value = newsItem.TimeAgo;
                    command.ExecuteNonQuery();
                }
            }

            connection.Close();
        }

        // Scrape the news.ycombinator.com main page
        private static List<NewsItem> ScrapeNewsYCombinator()
        {
            var newsItems = new List<NewsItem>();

            // Download the HTML of the news.ycombinator.com main page
            var webClient = new WebClient();
            var html = webClient.DownloadString("https://news.ycombinator.com/");

            // Parse the HTML to extract the news items
            var htmlDoc = new HtmlAgilityPack.HtmlDocument();
            htmlDoc.LoadHtml(html);
            var newsTable = htmlDoc.DocumentNode.SelectSingleNode("//table[@class='itemlist']");
            var newsRows = newsTable.SelectNodes("//tr[@class='athing']");
            foreach (var row in newsRows)
            {
                // Extract the title, URL, points, user, and time ago
                var titleLink = row.SelectSingleNode("td[@class='title']/a");
                var pointsSpan = row.SelectSingleNode("following-sibling::tr[1]//span[@class='score']");
                var userLink = row.SelectSingleNode("following-sibling::tr[1]//a[@class='hnuser']");
                var timeAgoSpan = row.SelectSingleNode("following-sibling::tr[1]//span[@class='age']");

                var newsItem = new NewsItem
                {
                    Title = titleLink.InnerText,
                    Url = titleLink.GetAttributeValue("href", ""),
                    Points = pointsSpan == null
                        ? 0
                        : int.Parse(pointsSpan.InnerText.Split(' ')[0]),
                    User = userLink == null
                        ? ""
                        : userLink.InnerText,
                    TimeAgo = timeAgoSpan.InnerText
                };
                newsItems.Add(newsItem);
            }

            return newsItems;
        }
    }

    class NewsItem
    {
        public string Title { get; set; }
        public string Url { get; set; }
        public int Points { get; set; }
        public string User { get; set; }
        public string TimeAgo { get; set; }
    }
}

```
