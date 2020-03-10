﻿<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Disease Solutions-Demographics</title>
<meta name="keywords" content="" />
<meta name="description" content="" />
<link href="tooplate_style.css" rel="stylesheet" type="text/css" />
<!--   Free Website Template by t o o p l a t e . c o m   -->
<script language="javascript" type="text/javascript">
function clearText(field)
{
    if (field.defaultValue == field.value) field.value = '';
    else if (field.value == '') field.value = field.defaultValue;
}
</script>

</head>
<body>

<div id="tooplate_wrapper">

	<div id="tooplate_header">
    	
        <div id="tooplate_menu">
        	<ul>
                <li><a href="index.php" >Home</a></li>
                <li><a href="aboutus.php">About Us</a></li>
				<li><a href="demographics.php" class="current">Demographics</a></li>
                <li><a href="simulator.php">Simulation</a></li>
				<li><a href="policies.php">Policies</a></li>
                <li><a href="contact.php">Contact</a></li>
            </ul>
            <div class="cleaner"></div>
        </div> <!-- end of menu -->
        <br></br>
		<div id="site_title"><h1><strong>Disease Solutions</strong><br></br><span>Leaders in Disaster Mitigation</span></h1></div>
    
    </div> <!-- end of header -->
    
    <div id="tooplate_middle">
   		
    </div> <!-- end of middle -->
    
	<div id="tooplate_main">
		<br></br>
		<h3>Disease Simulation Demographic Information</h3>
		<p>Use the following selections to gain more information about the data utilized in the simulation model. </p>
		
		<?php
			$servername = "mydb.ics.purdue.edu";
			$username = "g1081391";
			$password = "Wif1hasnomeaning";
			$dbname = "g1081391";

			// Create connection
			$conn = new mysqli($servername, $username, $password, $dbname);
							 
			// Check connection
			if ($conn->connect_error) {
				die("Connection failed: " . $conn->connect_error);
			} 
			
			$sql = "SELECT Region_Num FROM Region";
			$result = $conn->query($sql);
			
		?>
		<!--Display General Demographic Information-->
		<h5>General Population Demographics</h5>
		<center>
		
		<div class="demo">
			<p><em>Age and Race Distributions</em></p>
			<img src="images/gallery/AgesPlot.jpeg" />
			<img src="images/gallery/RacePlot.jpeg" />
			<p><em>School Populations and Public Transportation Utilization</em></p>
			<img src="images/gallery/Schoolsplot.jpeg" />
			<img src="images/gallery/PublicTransportPlot.jpeg"/>
		</div>
		
		<p><em>Population Density per Region</em></p>
		<img src="images/gallery/PopulationDensity.jpeg" width="550px"/>
		</center>
		<br></br>
		
		
		<h5>Region-Specific Demographics</h5>		
		<form id="regionForm" action = "demographics.php" method="post">
		<table class="order">
		<tr><td colspan=2></td></tr>
		<select name="Region" style="width:200px">
		<option selected="selected" disabled="disabled">Please select a region</option>
		<?php
			while($row = $result->fetch_assoc()) {	
				// output data of each row
				// https://davidwalsh.name/html-mysql-php
				// https://www.w3schools.com/html/html_tables.asp
				// https://stackoverflow.com/questions/9694508/php-mysql-footer-displayed-before-table-powered-by-a-database
				// https://stackoverflow.com/questions/23768051/add-border-to-table-within-echo-statement
				// https://stackoverflow.com/questions/6907751/select-count-from-table-of-mysql-in-php
				// https://www.formget.com/php-select-option-and-php-radio-button/
				echo "<option>" . $row['Region_Num'] . "</option>";
			}
			?>
			</select>
			<br></br>
			<input type="submit" name="submit" value="Select" />
			</form>
			<br></br>
			<?php
			if(isset($_POST['submit'])){
				$selected_val = $_POST['Region'];
				echo "You have selected: Region " .$selected_val;  // Displaying Selected Value
				echo "<table align=\"left\" class=\"db-table\" border = \"1\" width=875px>
				<tr><th>Parameter</th><th>Data</th></tr>";
			}
			// Population density based on region and posting to website
			$sql = "SELECT Region_Num, Pop_Density FROM Region WHERE Region_Num = $selected_val";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
				//echo "<table align=\"left\" class=\"db-table\" border = \"1\" width=875px>
				//<tr><th>Parameter</th><th>Data</th></tr>";

				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Region", "</td>";
					echo "<td width='300px'>", $row["Region_Num"], "</td>";
					echo "</tr>";
					echo "<tr>";
					echo "<td width='300px'>", "Population Density (people/sq mile)", "</td>";
					echo "<td width='300px'>", $row["Pop_Density"], "</td>";
					echo "</tr>";
				}
				
			} 
			// Number of households query and posting to website
			$sql = "SELECT COUNT(*) as total FROM Household WHERE Region_Num = $selected_val";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
								
				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Households", "</td>";
					echo "<td width='300px'>", $row["total"], "</td>";
					echo "</tr>";
				}
				
			}  
			// Average age of residents of a region query and posting to website 
			$sql = "SELECT AVG(Individuals.Age) As Average FROM Individuals, Household WHERE Individuals.Household_ID = Household.Household_ID AND Household.Region_Num = $selected_val GROUP BY Household.Region_Num";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
								
				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Average Age", "</td>";
					echo "<td width='300px'>", $row["Average"], "</td>";
					echo "</tr>";
				}
				
			} 
			// Most common race query per region and posting to website
			$sql = "SELECT Race, COUNT(Race) AS r FROM Household WHERE Region_Num = $selected_val GROUP BY Race ORDER BY r DESC LIMIT 1";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
								
				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Most Common Race", "</td>";
					echo "<td width='300px'>", $row["Race"], "</td>";
					echo "</tr>";
				}
				
			} 
			// Homes below poverty and posting to website
			$sql = "SELECT COUNT(Income) as count FROM Household WHERE Region_Num = $selected_val AND Income = 'POV'";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
				
				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Homes Below Poverty Level", "</td>";
					echo "<td width='300px'>", $row["count"], "</td>";
					echo "</tr>";
				}
				
			} 
			// Individuals below poverty level and posting to website
			$sql = "SELECT COUNT(*) as count FROM Individuals I, Household H WHERE H.Region_Num = $selected_val AND H.Income = 'POV' AND I.Household_ID = H.Household_ID";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
				
				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Individuals Living Below Poverty Level", "</td>";
					echo "<td width='300px'>", $row["count"], "</td>";
					echo "</tr>";
				}
				
			} 
			// Number of people that use public transporation and posting to website
			$sql = "SELECT COUNT(H.PubTrans) as total FROM Individuals I, Household H WHERE H.Region_Num = $selected_val AND H.PubTrans = 'Yes' and I.Household_ID = H.Household_ID";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
				
				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Number of People Regularly Using Public Transportation", "</td>";
					echo "<td width='300px'>", $row["total"], "</td>";
					echo "</tr>";
				}
				
			} 
			
			$sql = "SELECT AVG(Number_People) as average FROM Household WHERE Region_Num = $selected_val";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
				
				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Average Number of People per Household", "</td>";
					echo "<td width='300px'>", $row["average"], "</td>";
					echo "</tr>";
				}
				
			} 
			
			$sql = "SELECT AVG(Number_Children) as average FROM Household WHERE Region_Num = $selected_val";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
				
				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Average Number of Children per Household", "</td>";
					echo "<td width='300px'>", $row["average"], "</td>";
					echo "</tr>";
				}
				
			} 
			
			$sql = "SELECT AVG(Number_Old) as average FROM Household WHERE Region_Num = $selected_val";
			$result = $conn->query($sql);

			if ($result->num_rows > 0) {
				
				while($row = $result->fetch_assoc()) {
					echo "<tr>";
					echo "<td width='300px'>", "Average Number of Elderly per Household", "</td>";
					echo "<td width='300px'>", $row["average"], "</td>";
					echo "</tr>";
				}
				
			} 
			echo "</table><br><br><br><br><br><br><br><br><br><br><br><br>";
			$conn->close();
			
		?>
		</table>
		
	</div>	<!-- end of main -->
    
    <div id="tooplate_footer">
    
        Copyright © 2018 <em>Disease Solutions</em>
    
    </div> <!-- end of tooplate_footer -->

</div> <!-- end of wrapper -->
<!--   Free Website Template by t o o p l a t e . c o m   -->
</body>
</html>