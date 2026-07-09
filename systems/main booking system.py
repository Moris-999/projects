import customtkinter as ctk

# Set the overall theme and color style
ctk.set_appearance_mode("System")  # Options: "System", "Dark", "Light"
ctk.set_default_color_theme("blue")  # Options: "blue", "green", "dark-blue"

class ModernApp(ctk.CTk):
    def __init__(self):
        super().__init__()

        # Configure window
        self.title("Modern Tkinter Application")
        self.geometry("450x350")
        self.resizable(False, False)

        # Title Label
        self.title_label = ctk.CTkLabel(
            self, 
            text="Welcome to Your App", 
            font=ctk.CTkFont(size=20, weight="bold")
        )
        self.title_label.pack(padx=20, pady=20)

        # Text Input
        self.entry = ctk.CTkEntry(
            self, 
            placeholder_text="Type something here...", 
            width=300
        )
        self.entry.pack(padx=20, pady=10)

        # Action Button
        self.submit_btn = ctk.CTkButton(
            self, 
            text="Submit Entry", 
            command=self.handle_submit
        )
        self.submit_btn.pack(padx=20, pady=10)

        # Output Display Label
        self.output_label = ctk.CTkLabel(
            self, 
            text="", 
            font=ctk.CTkFont(size=14, italic=True)
        )
        self.output_label.pack(padx=20, pady=20)

        # Dark Mode Toggle Switch
        self.switch = ctk.CTkSwitch(
            self, 
            text="Dark Mode", 
            command=self.toggle_theme
        )
        self.switch.pack(side="bottom", pady=20)
        if ctk.get_appearance_mode() == "Dark":
            self.switch.select()

    def handle_submit(self):
        user_text = self.entry.get()
        if user_text:
            self.output_label.configure(text=f"Submitted: {user_text}")
            self.entry.delete(0, 'end')
        else:
            self.output_label.configure(text="Please enter some text first!")

    def toggle_theme(self):
        if self.switch.get() == 1:
            ctk.set_appearance_mode("Dark")
        else:
            ctk.set_appearance_mode("Light")

if __name__ == "__main__":
    app = ModernApp()
    app.mainloop()