from pypdf import PdfWriter, PdfReader
from pypdf.generic import NameObject

w = PdfWriter()
w.append(PdfReader("report.pdf"))
w.append(PdfReader("main.pdf"))
w._root_object.update({NameObject("/PageMode"): NameObject("/UseOutlines")})
with open("total.pdf", "wb") as f:
    w.write(f)
print(f"total.pdf: {sum(1 for _ in PdfReader('total.pdf').pages)} pages")
